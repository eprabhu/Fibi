package com.polus.fibicomp.manpowerintegration.scheduler;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.oxm.XmlMappingException;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
import org.springframework.util.ReflectionUtils;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.awardprojectoutcome.dao.AwardProjectOutcomeDao;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.ExcelityService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.manpower.dao.ManpowerDao;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.manpower.pojo.WorkdayManpowerInterface;
import com.polus.fibicomp.manpower.service.ManpowerService;
import com.polus.fibicomp.manpowerintegration.dao.ManpowerIntegrationDao;
import com.polus.fibicomp.manpowerintegration.dto.AwardClosePositionResorceDto;
import com.polus.fibicomp.manpowerintegration.pojo.AwardSupOrgMapping;
import com.polus.fibicomp.manpowerintegration.pojo.ManpowerLog;
import com.polus.fibicomp.manpowerintegration.service.ManpowerIntegrationService;
import com.polus.fibicomp.manpowerintegration.vo.ManpowerIntegrationVO;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.security.AuthenticatedUser;

@Component
@Service(value = "manpowerIntegrationSchedulerService")
public class ManpowerIntegrationSchedulerServiceImpl implements ManpowerIntegrationSchedulerService {

	protected static Logger logger = LogManager.getLogger(ManpowerIntegrationSchedulerServiceImpl.class.getName());

	@Autowired
	private ManpowerIntegrationDao manpowerIntegrationDao;

	@Autowired
	private ManpowerIntegrationService manpowerIntegrationService;

	private ExecutorService executorService = Executors.newCachedThreadPool();

	@Autowired
	private ManpowerService manpowerService;

	@Autowired
	private CommonDao commonDao;
	
	@Autowired
	private ManpowerDao manpowerDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private EmailService emailService;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private ExcelityService excelityService;

	@Value("${spring.application.name}")
	private String applicationURL;

	@Autowired
	private AwardProjectOutcomeDao awardProjectOutcomeDao;

	@Override
	public void workdayManpowerInterface(Award award, List<WorkdayManpowerInterface> workdayManpowerInterfaces, StringBuilder superiorSupOrg, Boolean isRetrigger) throws ParseException {
		logger.info("In workdayManpowerInterface");
		try {
			Map<String, String> workdayConfigDetails = manpowerIntegrationService.getWorkdayConfigDetails();
			WorkdayManpowerInterface manpowerInterface = manpowerIntegrationDao.fetchManpowerInterfaceByAwardNumberAndInterfaceTypeAndInterfaceStatus(award.getAwardNumber(), Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG, Constants.MANPOWER_INTERFACE_PENDING);
			if (manpowerInterface != null) {
				manpowerIntegrationService.checkAndCreateSupervisoryOrganizationId(award, workdayConfigDetails, manpowerInterface);
			}
			List<WorkdayManpowerInterface> manpowerInterfaces = workdayManpowerInterfaces;
			if (manpowerInterfaces == null || (manpowerInterfaces!= null && manpowerInterfaces.isEmpty())) {
				manpowerInterfaces = manpowerIntegrationDao.fetchManpowerInterfacesByAwardNumberAndInterfaceStatusAndInterfaceTypeCodesAndResourceUniqueId(award.getAwardNumber(), Constants.MANPOWER_INTERFACE_PENDING, null, null);
			}
			if (manpowerInterfaces != null && !manpowerInterfaces.isEmpty()) {
				String piPersonId = manpowerIntegrationDao.getPIPersonIdByAwardId(award.getAwardId());
				Integer activeAwardId = award.getAwardId();
				Timestamp systemDateTime = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
				int systemDay = getDayOfMonth(systemDateTime);
				int cutOffDay = Integer.parseInt(manpowerService.getManpowerConfigurationValueAsString(Constants.MANPOWER_COST_ALLOCATION_CUT_OFF_DATE));
				List<WorkdayManpowerInterface> allocationLessThanHundredInterfaces = new ArrayList<>();
				List<WorkdayManpowerInterface> allocationCutOffInterfaces = new ArrayList<>();
				List<WorkdayManpowerInterface> excludedManpowerInterfaces = new ArrayList<>();
				List<AwardManpowerResource> costAllocationSuccessResources = new ArrayList<>();
				StringBuilder freezeErrorMailContent = new StringBuilder();
				AtomicBoolean isProjectEndDateReduced = new AtomicBoolean(false);
				AtomicBoolean moveWorkerEventMail = new AtomicBoolean(false);
				AtomicBoolean isRetriggerApi = new AtomicBoolean(isRetrigger);
				StringBuilder moveWorkerExclusionMailContent = new StringBuilder();
				StringBuilder moveWorkerSuccessMailContent = new StringBuilder();
				StringBuilder moveWorkerErrorMailContent = new StringBuilder();
				manpowerInterfaces.stream().forEach(workdayManpowerInterface -> {
					String requestMessage = new StringBuilder("AwardNumber=").append(workdayManpowerInterface.getAwardNumber()).toString();
					String errorMessage = "";
					try {
						Integer count = 0;
						if (workdayManpowerInterface.getInterfaceTypeCode().equals(Constants.MANPOWER_INTERFACE_POSITION_CREATION)) {
							manpowerIntegrationService.createPosition(workdayManpowerInterface, workdayConfigDetails, superiorSupOrg.toString(), piPersonId, activeAwardId);
						} else if(workdayManpowerInterface.getInterfaceTypeCode().equals(Constants.MANPOWER_INTERFACE_FREEZE_POSITION)) {
							if (workdayManpowerInterface.getEndDateChange() < 0) {
								isProjectEndDateReduced.compareAndSet(false, true);
							}
							if (workdayManpowerInterface.getAwardManpowerResource() != null && workdayManpowerInterface.getAwardManpowerResource().getPositionId() != null) {
								count = manpowerIntegrationDao.getCountOfPositions(workdayManpowerInterface.getAwardManpowerResource().getPositionId());
							}
							if (count != null && count > 1) {
								excludedManpowerInterfaces.add(workdayManpowerInterface);
							} else {
								manpowerIntegrationService.freezePosition(workdayManpowerInterface, workdayConfigDetails, award, freezeErrorMailContent);
							}
						} else if (workdayManpowerInterface.getInterfaceTypeCode().equals(Constants.MANPOWER_INTERFACE_HRBP)) {
							manpowerIntegrationService.sendHrbpDetailsToPI(workdayManpowerInterface, workdayConfigDetails, superiorSupOrg.toString(), piPersonId, activeAwardId);
						} else if (workdayManpowerInterface.getInterfaceTypeCode().equals(Constants.MANPOWER_INTERFACE_COST_ALLOCATION)) {
							if (workdayManpowerInterface.getAwardManpowerResource().getCostAllocation().equals(new BigDecimal("100.00"))) {
								Timestamp allocationStartDate = workdayManpowerInterface.getAwardManpowerResource().getChargeStartDate() == null ? workdayManpowerInterface.getAwardManpowerResource().getPlanStartDate() : workdayManpowerInterface.getAwardManpowerResource().getChargeStartDate();
								if (!checkForCostAllocationCutOffCriteria(systemDay, cutOffDay, allocationStartDate, systemDateTime)) {
									manpowerIntegrationService.assignCostAllocation(workdayManpowerInterface, workdayConfigDetails, costAllocationSuccessResources, activeAwardId);
								} else {
									allocationCutOffInterfaces.add(workdayManpowerInterface);
								}
							} else {
								allocationLessThanHundredInterfaces.add(workdayManpowerInterface);
                                manpowerIntegrationService.saveWorkdayManpowerInterfaceWithMailFlag(workdayManpowerInterface, Constants.NO);
							}
						} else if (workdayManpowerInterface.getInterfaceTypeCode().equals(Constants.MANPOWER_INTERFACE_MOVE_WORKERS)) {
							if (Boolean.FALSE.equals(moveWorkerEventMail.get())) {
								if (Boolean.FALSE.equals(isRetriggerApi.get())) {
									String oldPiPersonId = workdayManpowerInterface.getOldPIPersonId();
									String newPiPersonId = workdayManpowerInterface.getNewPIPersonId();
									String oldSuperiorSupOrgId = workdayManpowerInterface.getOldSuperiorSupOrg();
									String newSuperiorSupOrgId = workdayManpowerInterface.getNewSuperiorSupOrg();
									manpowerIntegrationService.prepareNotificationForChangeInPIOrSuperiorSupOrgId(oldPiPersonId, newPiPersonId, oldSuperiorSupOrgId, newSuperiorSupOrgId, activeAwardId);
								}
								moveWorkerEventMail.compareAndSet(false, true);
							}
							if (workdayManpowerInterface.getAwardManpowerResource() != null) {
								AwardManpowerResource resource = workdayManpowerInterface.getAwardManpowerResource();
								Integer migratedPositionCount = manpowerIntegrationDao.getPositionCountWithCreateUser(resource.getPositionId());
								if (migratedPositionCount > 0) {
									moveWorkerExclusionMailContent.append("Position Id : ").append(resource.getPositionId()).append(",");
									if (resource.getPersonId() != null) {
										moveWorkerExclusionMailContent.append("Person Id : ").append(resource.getPersonId()).append(",");
									}
									moveWorkerExclusionMailContent.append("<br/>");
									manpowerIntegrationService.saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_FEED_NOT_NEEDED);
								} else if (resource.getPositionOwnedByAward() != null && resource.getPositionOwnedByAward().equals("Y")) {
									manpowerIntegrationService.moveWorkers(workdayManpowerInterface, workdayConfigDetails, activeAwardId, moveWorkerSuccessMailContent, moveWorkerErrorMailContent);
								} else {
									manpowerIntegrationService.saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_FEED_NOT_NEEDED);
								}
							}																																																
						}
					} catch (Exception e) {
						logger.error("Error in workdayManpowerInterface {} ", e.getMessage());
						e.printStackTrace();
						errorMessage = new StringBuilder("Error in workdayManpowerInterface ").append(e.getMessage()).toString();
						manpowerIntegrationService.saveManpowerLog(workdayManpowerInterface.getAwardNumber(), workdayManpowerInterface.getInterfaceTypeCode(), "FIBI_ERROR", errorMessage, 500, requestMessage, null, null, workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
					}
				});
				if (!freezeErrorMailContent.toString().isEmpty() && isProjectEndDateReduced.get()) {
					manpowerIntegrationService.prepareAndSendFreezeReducedMail(freezeErrorMailContent.toString(), award);

				}
   		        if (allocationLessThanHundredInterfaces != null && !allocationLessThanHundredInterfaces.isEmpty()) {
				  sendAllocationLessThanHundredMail(allocationLessThanHundredInterfaces);
				}
				if (allocationCutOffInterfaces != null && !allocationCutOffInterfaces.isEmpty()) {
					sendMailToCitsAfterCutOffDate(allocationCutOffInterfaces);
				}
				if (excludedManpowerInterfaces != null && !excludedManpowerInterfaces.isEmpty()) {
					sendExcludedFreezePositionNotification(excludedManpowerInterfaces, activeAwardId);
				}
				if (costAllocationSuccessResources != null && !costAllocationSuccessResources.isEmpty()) {
					sendMailForCommittedAmountExceedBudgetAmount(costAllocationSuccessResources, award);
				}
				if (Boolean.TRUE.equals(moveWorkerEventMail.get())) {
					manpowerIntegrationService.sendMoveWorkerNotificationMail(moveWorkerExclusionMailContent, moveWorkerSuccessMailContent, moveWorkerErrorMailContent, activeAwardId);
				}
			}
		} catch (Exception e) {
			logger.error("Error in method workdayManpowerInterface {} ", e.getMessage());
			e.printStackTrace();
		}
	}

	private void sendMailForCommittedAmountExceedBudgetAmount(List<AwardManpowerResource> costAllocationSuccessResources, Award award) {
		Map<String, List<AwardManpowerResource>> committedAmountExceededResources = costAllocationSuccessResources.stream().collect(Collectors.groupingBy(AwardManpowerResource::getAwardNumber));
		if (committedAmountExceededResources != null) {
			for (Entry<String, List<AwardManpowerResource>> manpowerResource: committedAmountExceededResources.entrySet()) {	
				StringBuilder mailContent = new StringBuilder("");
				List<AwardManpowerResource> manpowerResources = manpowerResource.getValue();
				manpowerResources.forEach(resource -> {
					Timestamp allocationStartDate = resource.getChargeStartDate() == null ? resource.getPlanStartDate() : resource.getChargeStartDate();
					Timestamp allocationEndDate = resource.getChargeEndDate() == null ? resource.getPlanEndDate() : resource.getChargeEndDate();
					mailContent.append("Full Name : ").append(resource.getFullName() == null ? "": resource.getFullName()).append("<br>")
					.append("Position Id : ").append(resource.getPositionId()).append("<br>")
					.append("Job Profile Name : ").append(resource.getManpowerJobProfileType() == null ? "" : resource.getManpowerJobProfileType().getDescription()).append("<br>")
					.append("Actual Start Date : ").append(commonService.convertDateFormatBasedOnTimeZone((allocationStartDate.getTime()),Constants.DEFAULT_DATE_FORMAT)).append("<br>")
					.append("Actual End Date : ").append(commonService.convertDateFormatBasedOnTimeZone((allocationEndDate.getTime()),Constants.DEFAULT_DATE_FORMAT)).append("<br>")
					.append("Cost Allocation % : ").append(resource.getCostAllocation()).append("<br>")
					.append("Initial Committed Amount : "+Constants.DOLLAR_SYMBOL +" ").append(resource.getPlannedSalary() == null ? "" : resource.getPlannedSalary()).append("<br>")
					.append("Actual Committed Amount : ").append(resource.getCommittedCost() == null ? "" : resource.getCommittedCost()).append("<br/><br/><br/>");
					});
				sendNotificationForCommittedAmountExceedBudgetAmount(manpowerResource.getKey(), mailContent, award);
			}
		}		
	}

	private void sendNotificationForCommittedAmountExceedBudgetAmount(String key, StringBuilder mailContent, Award award) {
		logger.info("Received request for sendNotificationForCommittedAmountExceedBudgetAmount mail sending");
		try {
			Map<String, String> placeHolder = new HashMap<>();
			Integer awardId = null;
			if (award != null) {
				awardId = award.getAwardId();
			} else {
				Award activeAward = awardDao.fetchActiveAwardByAwardNumber(key);
				if (activeAward != null) {
					awardId = activeAward.getAwardId();
				}
			}
			if (awardId != null) {
				EmailServiceVO emailServiceVO = new EmailServiceVO();
				emailServiceVO.setNotificationTypeId(Constants.MANPOWER_COMMITTED_AMOUNT_EXCEEDS_NOTIFICATION);
				emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
				emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
				emailServiceVO.setModuleItemKey(awardId.toString());
				emailServiceVO.setPlaceHolder(getPlaceHolderDataForCommittedAmountExceedMail(placeHolder, mailContent, awardId));
				emailService.sendEmail(emailServiceVO);
			}
		} catch(Exception e) {
			logger.info("Error in sendNotificationForCommittedAmountExceedBudgetAmount {}", e.getMessage());
			e.printStackTrace();
		} 
	}

	private Map<String, String> getPlaceHolderDataForCommittedAmountExceedMail(Map<String, String> placeHolder, StringBuilder mailContent, Integer awardId) {
		placeHolder.put("{MANPOWER_CONTENT}", mailContent.toString().replaceAll(",$", ""));
		placeHolder.put("{URL}", generateAwardManpowerLinkToApplication(awardId));
		return placeHolder;
	}

	public String generateAwardManpowerLinkToApplication(Integer awardId) {
		return Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_MANPOWER_PATH + awardId + Constants.APPLICATION_URL_END_TAG;
	}

	private void sendExcludedFreezePositionNotification(List<WorkdayManpowerInterface> excludedManpowerInterfaces, Integer activeAwardId) {
		StringBuilder mailContent = new StringBuilder();
		Map<String, String> placeHolder = new HashMap<>();
		logger.info("Received request for sendExcludedFreezePositionNotification mail sending");
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.EXCLUDED_FREEZE_POSITION_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(activeAwardId.toString());
		for (WorkdayManpowerInterface excludedManpowerInterface : excludedManpowerInterfaces) {
			mailContent.append(excludedManpowerInterface.getAwardManpowerResource().getPositionId()).append(",");
		}
		emailServiceVO.setPlaceHolder(getPlaceHolderData(placeHolder, mailContent));
		emailService.sendEmail(emailServiceVO);
		for (WorkdayManpowerInterface excludedManpowerInterface : excludedManpowerInterfaces) {
		  manpowerIntegrationService.saveWorkdayManpowerInterfaceWithInterfaceStatus(excludedManpowerInterface, Constants.MANPOWER_INTERFACE_SUCCESS);
		}
	}

	private Map<String, String> getPlaceHolderData(Map<String, String> placeHolder, StringBuilder mailContent) {
		placeHolder.put("{POSITION_NUMBER}", mailContent.toString().replaceAll(",$", ""));
		return placeHolder;
	}

	private void sendAllocationLessThanHundredMail(List<WorkdayManpowerInterface> allocationLessThanHundredInterfaces) {
		Map<String, List<WorkdayManpowerInterface>> workdayManpowerInterfaces = allocationLessThanHundredInterfaces.stream().collect(Collectors.groupingBy(WorkdayManpowerInterface::getAwardNumber));
		for (Map.Entry<String, List<WorkdayManpowerInterface>> mapElement : workdayManpowerInterfaces.entrySet()) {
			String awardNumber = mapElement.getKey();
			Award award = awardDao.fetchActiveAwardByAwardNumber(awardNumber);
			if (award == null) {
				award = awardDao.fetchPendingAwardByAwardNumber(awardNumber);
			}
			final Integer awardId = award.getAwardId();
			List<WorkdayManpowerInterface> manpowerInterfaces = mapElement.getValue();
			StringBuilder allocationContent = new StringBuilder("");
			manpowerInterfaces.forEach(manpowerInterface -> {
					allocationContent.append("Employee Id : ").append(manpowerInterface.getAwardManpowerResource().getPersonId()).append("<br/>");
					allocationContent.append("Full Name : ").append(manpowerInterface.getAwardManpowerResource().getFullName()).append("<br/>");
					allocationContent.append("Position Id : ").append(manpowerInterface.getAwardManpowerResource().getPositionId()).append("<br/>");
					allocationContent.append("Job Profile Name : ").append(manpowerInterface.getAwardManpowerResource().getManpowerJobProfileType() == null ? "" : manpowerInterface.getAwardManpowerResource().getManpowerJobProfileType().getDescription()).append("<br>");
					allocationContent.append("Proposed Base Salary  : ").append(manpowerInterface.getAwardManpowerResource().getPlannedBaseSalary()).append("<br/>");
					allocationContent.append("Level 2 WBS Number : ").append(manpowerInterface.getAwardManpower().getBudgetReferenceNumber()).append("<br/>");
					allocationContent.append("Cost Allocation Start Date : ").append(commonService.convertDateFormatBasedOnTimeZone((manpowerInterface.getAwardManpowerResource().getChargeStartDate() == null ? manpowerInterface.getAwardManpowerResource().getPlanStartDate().getTime() : manpowerInterface.getAwardManpowerResource().getChargeStartDate().getTime()),Constants.DEFAULT_DATE_FORMAT)).append("<br/>");
					allocationContent.append("Cost Allocation End Date : ").append(commonService.convertDateFormatBasedOnTimeZone(manpowerInterface.getAwardManpowerResource().getChargeEndDate() == null ? manpowerInterface.getAwardManpowerResource().getPlanEndDate().getTime() : manpowerInterface.getAwardManpowerResource().getChargeEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT)).append("<br/>");
					allocationContent.append("Cost Allocation Percentage : ").append(manpowerInterface.getAwardManpowerResource().getCostAllocation()).append("<br/>");
					allocationContent.append("Proposed Committed Amount : ").append(Constants.DOLLAR_SYMBOL +" ").append(manpowerInterface.getAwardManpowerResource().getPlannedSalary() != null ? manpowerInterface.getAwardManpowerResource().getPlannedSalary() : "").append("<br/>");
					allocationContent.append("Comments : ").append(manpowerInterface.getAwardManpowerResource().getDescription() != null ? manpowerInterface.getAwardManpowerResource().getDescription() : "").append("<br/>");
					allocationContent.append("<br/><br/>");
			});
			if (!allocationContent.toString().equals("")) {
				manpowerIntegrationService.prepareAndSendAllocationLessThanHundredMail(allocationLessThanHundredInterfaces, allocationContent, awardId);
			}
		}
	}

	private boolean checkForCostAllocationCutOffCriteria(int systemDay, int cutOffDay, Timestamp allocationStartDate, Timestamp systemDateTime) {
		return (systemDay > cutOffDay && (((allocationStartDate.before(systemDateTime) && getMonthFromTimestamp(allocationStartDate) == getMonthFromTimestamp(systemDateTime) && getYearFromTimestamp(allocationStartDate) == getYearFromTimestamp(systemDateTime)) || allocationStartDate.equals(systemDateTime) || (allocationStartDate.after(systemDateTime) && getMonthFromTimestamp(allocationStartDate) == getMonthFromTimestamp(systemDateTime) && getYearFromTimestamp(allocationStartDate) == getYearFromTimestamp(systemDateTime)))));
	}

	private int getYearFromTimestamp(Timestamp timestamp) {
		return timestamp.toLocalDateTime().toLocalDate().getYear();
	}

	private int getMonthFromTimestamp(Timestamp timestamp) {
		return timestamp.toLocalDateTime().toLocalDate().getMonthValue();
	}

	private int getDayOfMonth(Timestamp timestamp) {
		return timestamp.toLocalDateTime().toLocalDate().getDayOfMonth();
	}

	@Scheduled(cron = "${workday.jobProfile.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void getAllJobProfiles() {
		try {
			StringBuilder schedulerMailContent = new StringBuilder();
			schedulerMailContent.append("Scheduler Started for All Job Profiles at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			AtomicInteger rowCount = new AtomicInteger(0);
			logger.info("running scheduler for getAllManpowerDetails at : {}",commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
			manpowerIntegrationService.getWorkdayJobProfile(rowCount);
			schedulerMailContent.append("Number of Row(s) Processed : ").append(rowCount).append("<br>");
			schedulerMailContent.append("Scheduler Ended for All Job Profiles at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			sendSchedulerRunningTimeMail(schedulerMailContent.toString());
		} catch (Exception e) {
			logger.error("error in getAllJobProfiles : {}", e.getMessage());
		}
	}

	@Scheduled(cron = "${workday.manpowerDetails.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void getAllManpowerDetails() {
		try {
			StringBuilder schedulerMailContent = new StringBuilder();
			schedulerMailContent.append("Scheduler Started for All Manpower Details at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			AtomicInteger rowCount = new AtomicInteger(0);
			logger.info("running scheduler for getAllManpowerDetails at : {}", commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
			manpowerIntegrationService.getManpowerDetails(rowCount);
			schedulerMailContent.append("Number of Row(s) Processed : ").append(rowCount).append("<br>");
			schedulerMailContent.append("Scheduler Ended for All Manpower Details at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			sendSchedulerRunningTimeMail(schedulerMailContent.toString());
		}catch (Exception e) {
			logger.error("error in getAllManpowerDetails : {}", e.getMessage());
		}
	}

	@Scheduled(cron = "${workday.citizeshipNationality.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void getCitizenshipNationalityDetails() {
		try {
			StringBuilder schedulerMailContent = new StringBuilder();
			schedulerMailContent.append("Scheduler Started for Citizenship Nationality Details at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			AtomicInteger rowCount = new AtomicInteger(0);
			logger.info("running scheduler for getCitizenshipNationalityDetails at : {}", commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
			manpowerIntegrationService.getNationalityDetails(rowCount);
			schedulerMailContent.append("Number of Row(s) Processed : ").append(rowCount).append("<br>");
			schedulerMailContent.append("Scheduler Ended for Citizenship Nationality Details at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			sendSchedulerRunningTimeMail(schedulerMailContent.toString());
		}catch (Exception e) {
			logger.error("error in getCitizenshipNationalityDetails : {}", e.getMessage());
		}
	}

	@Override
	public void checkForManpowerIntegration(Award award, Award activeAward, AwardPerson newAwardPIPerson, AwardPerson activeAwardPIPerson, String activeAwardSuperiorSupOrg) {
		logger.info("Requesting for checkForManpowerIntegration");
		if(manpowerIntegrationService.checkIfAwardHasManpowerStaffResource(award.getAwardId()) || (activeAward != null ? manpowerIntegrationService.checkIfAwardHasManpowerStaffResource(activeAward.getAwardId()) : false)) {
			logger.info("Requesting for saveAwardChangesForIntegration");
			saveAwardChangesAndIntegrate(award, activeAward, true, null, newAwardPIPerson, activeAwardPIPerson, activeAwardSuperiorSupOrg);
		}
	}

	private void saveAwardChangesAndIntegrate(Award award, Award activeAward, Boolean awardSubmit, List<WorkdayManpowerInterface> workdayManpowerInterfaces, AwardPerson newAwardPIPerson, AwardPerson activeAwardPIPerson, String activeAwardSuperiorSupOrg) {
		try {
			if (awardSubmit.equals(Boolean.TRUE)) {
				manpowerService.saveWorkdayManpowerInterfaceDetails(award, activeAward, newAwardPIPerson, activeAwardPIPerson, activeAwardSuperiorSupOrg);
			}
		} catch (Exception e) {
			logger.error("Error in saveAwardChangesAndIntegrate {}", e.getMessage());
			e.printStackTrace();
			throw new ApplicationException("Error in saveAwardChangesAndIntegrate", e, Constants.JAVA_ERROR);
		}
	}

	@Scheduled(cron = "${workday.closePosition.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void closePosition() {
		List<AwardClosePositionResorceDto> closePositionResources = new ArrayList<>();
		closePositionResources(closePositionResources);
	}

	private void closePositionResources(List<AwardClosePositionResorceDto> closePositionManpowerResources) {
		try {
			StringBuilder schedulerMailContent = new StringBuilder();		
			schedulerMailContent.append("Scheduler Started for Close Position at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			AtomicInteger rowCount = new AtomicInteger(0);
			logger.info("running scheduler for closePosition at : {}", commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
			if (closePositionManpowerResources.isEmpty()) {
				closePositionManpowerResources = manpowerIntegrationDao.getClosePositionAwardPositionsList();
			}
			if (closePositionManpowerResources != null && !closePositionManpowerResources.isEmpty()) {
				Map<String, String> workdayConfigDetails = manpowerIntegrationService.getWorkdayConfigDetails();
				Map<String, List<AwardClosePositionResorceDto>> schoolManpowerResources = closePositionManpowerResources.stream().collect(Collectors.groupingBy(AwardClosePositionResorceDto::getSchool));
					for (Entry<String, List<AwardClosePositionResorceDto>> schoolManpowerResource : schoolManpowerResources.entrySet()) {
						List<AwardClosePositionResorceDto> manpowerResources = schoolManpowerResource.getValue();
						if (manpowerResources != null && !manpowerResources.isEmpty()) {
							StringBuilder mailContent = new StringBuilder();
							Map<String, List<AwardClosePositionResorceDto>> awardManpowerResources = manpowerResources.stream().collect(Collectors.groupingBy(AwardClosePositionResorceDto::getAwardNumber));
							Integer awardCount = 1;
							for (Entry<String, List<AwardClosePositionResorceDto>> awardManpowerResource : awardManpowerResources.entrySet()) {
								mailContent.append("<b>------Award ").append(awardCount).append(" ------</b>").append("<br>");
								List<AwardClosePositionResorceDto> resources = awardManpowerResource.getValue();
								Award award =  awardDao.getAwardDetailsById(resources.get(0).getAwardId());
								if (award != null) {
									AwardPerson piAwardPerson = awardProjectOutcomeDao.getAwardPiDetails(award.getAwardId());
									mailContent.append("Award Number : ").append(award.getAwardNumber() != null ? award.getAwardNumber() : "").append("<br>")
									.append("Account Number : ").append(award.getAccountNumber() != null ? award.getAccountNumber() : "").append("<br>")
									.append("Principal Investigator : ").append(piAwardPerson != null && piAwardPerson.getFullName() != null ? piAwardPerson.getFullName() : "").append("<br>")
									.append("Lead Unit : ").append(award.getLeadUnit() != null ? award.getLeadUnit().getUnitName() : "").append("<br>")
									.append("Award Start Date : ").append(award.getBeginDate() != null ? commonService.convertDateFormatBasedOnTimeZone(award.getBeginDate().getTime(),Constants.DEFAULT_DATE_FORMAT) : "").append("<br>")
									.append("Award End Date : ").append(award.getFinalExpirationDate() != null ? commonService.convertDateFormatBasedOnTimeZone(award.getFinalExpirationDate().getTime(),Constants.DEFAULT_DATE_FORMAT) : "").append("<br>");
									AwardSupOrgMapping awardSupOrgMapping = null;
									if (piAwardPerson != null && piAwardPerson.getPersonId() != null) {
										awardSupOrgMapping = manpowerIntegrationDao.getLatestAwardSupOrgByPI(award.getAwardNumber(), piAwardPerson.getPersonId());
									}
									mailContent.append("Sup Org Id : ").append(awardSupOrgMapping != null && awardSupOrgMapping.getSupOrgId() != null ? awardSupOrgMapping.getSupOrgId() : "").append("<br><br>");
								}
								StringBuilder singlePositionsOwned = new StringBuilder();
								StringBuilder errorMessage = new StringBuilder();
								StringBuilder multipleOwnedPositions = new StringBuilder();
								StringBuilder notOwnedPositions = new StringBuilder();
								if (resources != null && !resources.isEmpty()) {
									resources.stream().forEach(resource -> {
										if (resource.getPositionOwned().equals("SINGLE")) {
											singlePositionsOwned.append(resource.getPositionId()).append(",");
											try {
												if (Boolean.TRUE.equals(manpowerIntegrationService.closeWorkdayPosition(resource.getPositionId(), resource.getAwardNumber(), errorMessage, workdayConfigDetails, resource.getAwardId()))) {
													List<String> statuses = new ArrayList<>();
													statuses.add(Constants.MANPOWER_POSITION_GENERATED);
													statuses.add(Constants.MANPOWER_PENDING_APPROVAL);
													statuses.add(Constants.MANPOWER_ACTIVE);
													statuses.add(Constants.MANPOWER_HIRING_ON_EXISTING_POSITION);
													manpowerIntegrationDao.getManpowerResourceByPostionIdAndStatuses(resource.getPositionId(), statuses).stream().forEach(manpowerResource -> {
																rowCount.getAndIncrement();
																manpowerResource.setPositionStatusCode(Constants.MANPOWER_ENDED);
																manpowerDao.saveOrUpdateAwardManpowerResources(manpowerResource);
															});
												}
											} catch (XmlMappingException e) {
												logger.error("error occuered in close position due to XmlMappingException: {}",
														e.getMessage());
											} catch (Exception e) {
												logger.error("error occuered in close position : {}", e.getMessage());
											}
										} else if (resource.getPositionOwned().equals("MULIPLE")) {
											multipleOwnedPositions.append(resource.getPositionId()).append(",");
										} else if (resource.getPositionOwned().equals("NIL")) {
											notOwnedPositions.append(resource.getPositionId()).append(",");
										}
									});
								}
								awardCount = awardCount + 1;
								mailContent.append("Position Ownership only in this Award for Close Position API: ").append("<br>");
								mailContent.append((singlePositionsOwned != null && !singlePositionsOwned.toString().isEmpty()) ? singlePositionsOwned.toString().replaceAll(",$", "") : "Nil").append("<br><br>");
								mailContent.append("Positions which returned error : ").append("<br>");
								mailContent.append((errorMessage != null && !errorMessage.toString().isEmpty()) ? errorMessage : "Nil").append("<br>");
								mailContent.append("Excluded Positions (Reason being Position Ownership in multiple Awards) : ").append("<br>");
								mailContent.append((multipleOwnedPositions != null && !multipleOwnedPositions.toString().isEmpty()) ? multipleOwnedPositions.toString().replaceAll(",$", "") : "Nil").append("<br><br>");
								mailContent.append("Excluded Positions (Reason being No Position Ownership in any Awards) : ").append("<br>");
								mailContent.append((notOwnedPositions != null && !notOwnedPositions.toString().isEmpty()) ? notOwnedPositions.toString().replaceAll(",$", "") : "Nil").append("<br><br><br>");
							}
							sendEmailForClosePosition(mailContent, manpowerResources.get(0).getAwardId());
						}
					}
			}
			schedulerMailContent.append("Number of Row(s) Processed : ").append(rowCount).append("<br>");
			schedulerMailContent.append("Scheduler Ended for Close Position at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			sendSchedulerRunningTimeMail(schedulerMailContent.toString());
		}catch (Exception e) {
			logger.error("error occuered in closePosition : {}", e.getMessage());
		}
	}

	private void sendEmailForClosePosition(StringBuilder mailContent, Integer awardId) {
		logger.info("Requesting for Close Position Mail sending");
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		Map<String, String> placeHolder = new HashMap<>();
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_CLOSE_POSITION_NOTIFICATION);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setModuleItemKey(awardId.toString());
		emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
		placeHolder.put("{CLOSE_POSITION_CONTENT}", mailContent.toString());
		emailServiceVO.setPlaceHolder(placeHolder);
		emailService.sendEmail(emailServiceVO);
	}

	@Scheduled(cron = "${workday.costAllocation.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void assignCostAllocation() {
		List<WorkdayManpowerInterface> interfaces = new ArrayList<>();
		assignCostAllocationDetails(interfaces);
	}

	@Override
	public void assignCostAllocationDetails(List<WorkdayManpowerInterface> interfaces) {
		try {
			logger.info("running assignCostAllocation at : {}", commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
			List<AwardManpowerResource> costAllocationSuccessResources = new ArrayList<>();
			List<WorkdayManpowerInterface> manpowerInterfaces = new ArrayList<>();
			if (interfaces.isEmpty()) {
				manpowerInterfaces =  manpowerIntegrationDao.fetchManpowerInterfacesForCostAllocationAfterManpower(Constants.MANPOWER_INTERFACE_PENDING, Constants.MANPOWER_INTERFACE_COST_ALLOCATION_AFTER_MANPOWER);
			} else {
				manpowerInterfaces = interfaces;
			}
			List<WorkdayManpowerInterface> allocationCutOffInterfaces = new ArrayList<>();
			if (manpowerInterfaces != null && !manpowerInterfaces.isEmpty()) {
				Timestamp systemDateTime = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
				int systemDay = getDayOfMonth(systemDateTime);
				int cutOffDay = Integer.parseInt(manpowerService.getManpowerConfigurationValueAsString(Constants.MANPOWER_COST_ALLOCATION_CUT_OFF_DATE));
				Map<String, String> workdayConfigDetails = manpowerIntegrationService.getWorkdayConfigDetails();
				List<WorkdayManpowerInterface> allocationLessThanHundredInterfaces = new ArrayList<>();
				manpowerInterfaces.forEach(manpowerInterface -> {
					try {
						if (manpowerInterface.getAwardManpowerResource().getCostAllocation().equals(new BigDecimal("100.00"))) {
							Timestamp chargeStartDate = manpowerInterface.getAwardManpowerResource().getChargeStartDate();
							if (!checkForCostAllocationCutOffCriteria(systemDay, cutOffDay, chargeStartDate, systemDateTime)) {
								Award activeAward = awardDao.fetchActiveAwardByAwardNumber(manpowerInterface.getAwardNumber());
								manpowerIntegrationService.assignCostAllocation(manpowerInterface, workdayConfigDetails, costAllocationSuccessResources, activeAward.getAwardId());
							} else {
								allocationCutOffInterfaces.add(manpowerInterface);
							}
						} else {
							allocationLessThanHundredInterfaces.add(manpowerInterface);
							manpowerIntegrationService.saveWorkdayManpowerInterfaceWithMailFlag(manpowerInterface, Constants.NO);
						}
						
					} catch (Exception e ) {
						logger.error("Error in assignCostAllocation : {}", e.getMessage());
					}
				});
				if (allocationCutOffInterfaces != null && !allocationCutOffInterfaces.isEmpty()) {
					sendMailToCitsAfterCutOffDate(allocationCutOffInterfaces);
				}
				if (allocationLessThanHundredInterfaces != null && !allocationLessThanHundredInterfaces.isEmpty()) {
					sendAllocationLessThanHundredMail(allocationLessThanHundredInterfaces);
				}
				if (costAllocationSuccessResources != null && !costAllocationSuccessResources.isEmpty()) {
					sendMailForCommittedAmountExceedBudgetAmount(costAllocationSuccessResources, null);
				}
			} else {
				logger.info("No cost allocations in queue");
				manpowerIntegrationService.saveManpowerLog(null, Constants.MANPOWER_INTERFACE_COST_ALLOCATION_AFTER_MANPOWER, "SUCCESS", "No cost allocations in queue", 200, null, null, null, null, null);
			}
		}catch (Exception e) {
			logger.error("error occuered in assignCostAllocation : {}", e.getMessage());
		}
	}

	@Scheduled(cron = "${workday.costReconciliation.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void startCostingAllocationReconciliation() {
		try {
			StringBuilder schedulerMailContent = new StringBuilder();		
			List<AwardManpowerResource> costAllocationSuccessResources = new ArrayList<>();
			schedulerMailContent.append("Scheduler Started for Cost Reconciliation at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			AtomicInteger rowCount = new AtomicInteger(0);
			logger.info("running scheduler for costReconciliation at : {}",commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
			Map<String, List<AwardManpowerResource>> manpowerResources = manpowerIntegrationDao.getManpoerResourceByActualAmountNull().stream().collect(Collectors.groupingBy(AwardManpowerResource::getPersonId));
			Map<String, String> workdayConfigDetails = manpowerIntegrationService.getWorkdayConfigDetails();
			if (manpowerResources != null && !manpowerResources.isEmpty()) {
				StringBuilder finalMailContent = null;
				StringBuilder mailContent = null;
				StringBuilder mailContentSubject = null;
				for (Entry<String, List<AwardManpowerResource>> manpowerResource: manpowerResources.entrySet()) {				
					mailContent = new StringBuilder();
					finalMailContent = new StringBuilder();
					mailContentSubject = new StringBuilder();
					mailContentSubject.append(manpowerResource.getKey() != null ? manpowerResource.getKey() : "");
					AtomicBoolean success = new AtomicBoolean(false);
					AtomicBoolean isRise = new AtomicBoolean(false);
					AtomicBoolean isWorkday = new AtomicBoolean(false);
					Set<String> uniqueId = new HashSet<>();
					Set<String> successUniqueId = new HashSet<>();
					String content = manpowerIntegrationService.getCostingAllocationReconciliation(manpowerResource, workdayConfigDetails, success, isWorkday, isRise, mailContent, rowCount, uniqueId, successUniqueId, costAllocationSuccessResources);
					if(content != null && content.length() != 0) {						
					finalMailContent.append(content);
					sendReconcilationMail(finalMailContent.toString(),mailContentSubject.toString());
					}
				}			
			} else {
				logger.info("No cost allocations to reconcile");
				manpowerIntegrationService.saveManpowerLog(null, Constants.MANPOWER_INTERFACE_COST_RECONCILATION, "SUCCESS", "No cost allocations to reconcile", 200, null, null, null, null, null);
			}
			if (costAllocationSuccessResources != null && !costAllocationSuccessResources.isEmpty()) {
				sendMailForCommittedAmountExceedBudgetAmount(costAllocationSuccessResources, null);
			}
			schedulerMailContent.append("Number of Row(s) Processed : ").append(rowCount).append("<br>");
			schedulerMailContent.append("Scheduler Ended for Cost Reconciliation at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			sendSchedulerRunningTimeMail(schedulerMailContent.toString());
		}catch (Exception e) {
			logger.error("error occuered in startCostingAllocationReconciliation : {}", e.getMessage());
		}
	}

	private void sendSchedulerRunningTimeMail(String schedulerMailContent) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_SCHEDULER_MAIL_NOTIFICATION);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.MANPOWER_SUBMODULE_CODE.toString());
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{MANPOWER_SCHEDULER_MAIL}", schedulerMailContent);
		emailServiceVO.setPlaceHolder(placeHolder);
		emailService.sendEmail(emailServiceVO);
		
	}

	private void sendReconcilationMail(String finalMailContent, String mailContentSubject) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_COST_RECONCILATION_NOTIFICATION);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{PERSON_ID}", mailContentSubject);
		placeHolder.put("{RECONCILATION_CONTENT}", finalMailContent);
		emailServiceVO.setPlaceHolder(placeHolder);
		emailService.sendEmail(emailServiceVO);
	}

	@Override
	public void sendMailToCitsAfterCutOffDate(List<WorkdayManpowerInterface> allocationCutOffInterfaces) {
		StringBuilder finalMailContent = new StringBuilder("");
		try {
			Map<String, List<WorkdayManpowerInterface>> workdayManpowerInterfaces = allocationCutOffInterfaces.stream().collect(Collectors.groupingBy(WorkdayManpowerInterface::getAwardNumber));
			for (Map.Entry<String, List<WorkdayManpowerInterface>> mapElement : workdayManpowerInterfaces.entrySet()) {
				String awardNumber = mapElement.getKey();
				Award award = awardDao.fetchActiveAwardByAwardNumber(awardNumber);
				String piName = manpowerIntegrationDao.getPIFullNameByAwardId(award.getAwardId());
				finalMailContent.append("<i><b>---AwardNumber : ").append(awardNumber).append("---<b></i><br>")
				.append("Account Number : ").append(award.getAccountNumber() == null ? "" : award.getAccountNumber()).append("<br>")
				.append("PI Name : ").append(piName).append("<br>")
				.append("Lead Unit: ").append(award.getLeadUnit().getUnitName()).append("<br><br>");
				List<WorkdayManpowerInterface> manpowerInterfaces = mapElement.getValue();
				manpowerInterfaces.forEach(manpowerInterface -> {
					Timestamp allocationStartDate = manpowerInterface.getAwardManpowerResource().getChargeStartDate() == null ? manpowerInterface.getAwardManpowerResource().getPlanStartDate() : manpowerInterface.getAwardManpowerResource().getChargeStartDate();
					Timestamp allocationEndDate = manpowerInterface.getAwardManpowerResource().getChargeEndDate() == null ? manpowerInterface.getAwardManpowerResource().getPlanEndDate() : manpowerInterface.getAwardManpowerResource().getChargeEndDate();
					finalMailContent.append("Employee Id : ").append(manpowerInterface.getAwardManpowerResource().getPersonId()).append("<br>")
					.append("Charge Start Date : ").append(allocationStartDate).append("<br>")
					.append("Charge End Date : ").append(allocationEndDate).append("<br>")
					.append("ProjectId/Level2 WBS Number : ")
					.append(manpowerInterface.getAwardManpower().getBudgetReferenceNumber() == null ? "" : manpowerInterface.getAwardManpower().getBudgetReferenceNumber()).append("<br><br>");
				});
			}
			sendMailToCits(finalMailContent);
			allocationCutOffInterfaces.forEach(manpowerInterface -> {
				manpowerIntegrationService.saveWorkdayManpowerInterfaceWithInterfaceStatus(manpowerInterface, Constants.MANPOWER_INTERFACE_ERROR);
			});
		} catch (Exception e) {
			logger.error("Error in sendMailToCitsAfterCutOffDate: {}", e.getMessage());
		}
	}

	private void sendMailToCits(StringBuilder mailContent) {
		logger.info("Requesting for cost allocation cutoff mail sending");
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_INTEGRATION_CUTOFF_CITS_NOTIFICATION);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{INTEGRATION_CUTOFF_CONTENT}", mailContent.toString());
		emailServiceVO.setPlaceHolder(placeHolder);
		emailService.sendEmail(emailServiceVO);
	}
	
	@Scheduled(cron = "${workday.manpowerLogMail.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void getManpowerLogMail() {
		try {
			StringBuilder schedulerMailContent = new StringBuilder();		
			schedulerMailContent.append("Scheduler Started for ManpowerLogMail at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			AtomicInteger rowCount = new AtomicInteger(0);
			logger.info("running scheduler for getManpowerLogMail at : {}", commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
			List<ManpowerLog> manpowerLogList = manpowerIntegrationDao.getManpowerLogDetails(Constants.MANPOWER_LOG_MESSAGE_TYPE);
			if (manpowerLogList != null && !manpowerLogList.isEmpty()) {
				StringBuilder mailContent = new StringBuilder();
				manpowerLogList.forEach(manpowerLog -> {
					try {
						rowCount.getAndIncrement();
						String piName = "";
						Award award = null;
						String awardNumber = manpowerLog.getAwardNumber();
						if (awardNumber != null) {
							award = awardDao.fetchActiveAwardByAwardNumber(awardNumber);
							if (award == null) {
								award = awardDao.fetchPendingAwardByAwardNumber(awardNumber);
							}
							if (award != null) {
								piName = manpowerIntegrationDao.getPIFullNameByAwardId(award.getAwardId());
								mailContent.append("Award Number : ").append(award.getAwardNumber() != null ? award.getAwardNumber() : "").append("<br>")
								.append("Account Number : ").append(award.getAccountNumber() == null ? "" : award.getAccountNumber()).append("<br>")
								.append("PI Name : ").append(piName).append("<br>")
								.append("Lead Unit : ").append(award.getLeadUnit().getUnitName()).append("<br>");
							}
						}
						mailContent.append("Request : ").append(manpowerLog.getRequestParam() != null ? manpowerLog.getRequestParam() : "").append("<br>")
						.append(manpowerLog.getMessage() != null ? manpowerLog.getMessage() : "").append("<br>")
						.append("Date : ").append(manpowerLog.getUpdateTimeStamp() != null ? manpowerLog.getUpdateTimeStamp().toString() : "").append("<br><br>");
					} catch (Exception e) {
						logger.info("Error in getManpowerLog ", e.getMessage());
					}
				});
				sendManpowerLogEmail(mailContent.toString());
			}
			schedulerMailContent.append("Number of Row(s) Processed : ").append(rowCount).append("<br>");
			schedulerMailContent.append("Scheduler Ended for ManpowerLogMail at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			sendSchedulerRunningTimeMail(schedulerMailContent.toString());
		} catch (Exception e) {
			logger.error("Error in getManpowerLogMail: {}", e.getMessage());
		}
	}
	
	private void sendManpowerLogEmail(String mailContent) {
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_LOG_MAIL_NOTIFICATOIN_CODE);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		emailServiceVO.setRecipients(dynamicEmailrecipients);
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{MANPOWER_LOG_EMAIL}", mailContent);
		emailServiceVO.setPlaceHolder(placeHolder);
		emailService.sendEmail(emailServiceVO);
	}

	@Scheduled(cron = "${workday.longLeave.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void startWorkdayLongLeave() {
		try {
			StringBuilder schedulerMailContent = new StringBuilder();
			schedulerMailContent.append("Scheduler Started for Workday Long Leave at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			AtomicInteger rowCount = new AtomicInteger(0);
			logger.info("running scheduler for StartWorkdayLongLeave at : {}", commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
			manpowerIntegrationService.getWorkdayLongLeave(rowCount);
			schedulerMailContent.append("Number of Row(s) Processed : ").append(rowCount).append("<br>");
			schedulerMailContent.append("Scheduler Ended for Workday Long Leave at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			sendSchedulerRunningTimeMail(schedulerMailContent.toString());
		}catch (Exception e) {
			logger.error("Error in startWorkdayLongLeave: {}", e.getMessage());
		}
	}

	@Scheduled(cron = "${workday.terminations.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void startWorkdayTerminations() {
		try {
			StringBuilder schedulerMailContent = new StringBuilder();
			schedulerMailContent.append("Scheduler Started for Workday Terminations at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			AtomicInteger rowCount = new AtomicInteger(0);
			logger.info("running scheduler for startWorkdayTerminations at : {}", commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
			manpowerIntegrationService.getWorkdayTerminations(rowCount);
			schedulerMailContent.append("Number of Row(s) Processed : ").append(rowCount).append("<br>");
			schedulerMailContent.append("Scheduler Ended for Workday Terminations at : ").append(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)).append("<br>");
			sendSchedulerRunningTimeMail(schedulerMailContent.toString());
		}catch (Exception e) {
			logger.error("Error in startWorkdayTerminations: {}", e.getMessage());
		}
	}

	@Override
	public void encryptAllMigratedCitizenshipNationality() {
		try {
			logger.info("Manpower details copying from Migrated table to Manpower table start at : {}", commonDao.getCurrentTimestamp());
			manpowerIntegrationDao.getallMigratedManpowerPersons().stream().forEach(migratedPerson -> {
				Manpower manpower = manpowerIntegrationDao.getManpowerByPersonId(migratedPerson.getWorkerPersonId());
				if (manpower != null) {
					try {
						if (migratedPerson.getCitizenship() != null || migratedPerson.getNationality() != null) {
							if (migratedPerson.getCitizenship() != null) {
								manpower.setCitizenship(excelityService.encryptAES(migratedPerson.getCitizenship()));
							}
							if (migratedPerson.getNationality() != null) {
								manpower.setNationality(excelityService.encryptAES(migratedPerson.getNationality()));
							}
							manpower.setUpdateUser("quickstart");
							manpower.setUpdateTimestamp(commonDao.getCurrentTimestamp());
							manpowerIntegrationDao.saveOrUpdate(manpower);
						}
					} catch (Exception e) {
						logger.info("Error Occurred while encrypting data: {} ", migratedPerson.getWorkerPersonId());
					}
				}
			});
			logger.info(" Manpower details copying from Migrated table to Manpower table ended at : {}", commonDao.getCurrentTimestamp());
		} catch (Exception e) {
			logger.error("Error Occurred in getManPowerDetails: {}", e.getMessage());
		}
	}

	@Scheduled(cron = "${workday.designationChange.api}", zone = Constants.CRON_JOB_TIMEZONE)
	public void getJobProfileChanges() {
		manpowerIntegrationService.getJobProfileChanges();
	}

	@Override
	public String retriggerAwardWorkdayPrerequisite(ManpowerIntegrationVO vo) {
		WorkdayManpowerInterface workdayManpowerInterfaceDetail = new WorkdayManpowerInterface();
		Map<String, String> workdayConfigDetails = manpowerIntegrationService.getWorkdayConfigDetails();
		WorkdayManpowerInterface workdayManpowerInterface = manpowerIntegrationDao.getWorkdayManpowerInterfaceById(vo.getWorkdayManpowerInterfaceId());
		ReflectionUtils.shallowCopyFieldState(workdayManpowerInterface, workdayManpowerInterfaceDetail);
		workdayManpowerInterfaceDetail.setWorkdayManpowerInterfaceId(null);
		workdayManpowerInterfaceDetail = saveWorkdayManpowerInterface(workdayManpowerInterfaceDetail);
		Award award = awardDao.fetchActiveAwardByAwardNumber(workdayManpowerInterface.getAwardNumber());
		String message = manpowerIntegrationService.checkAndCreateSupervisoryOrganizationId(award, workdayConfigDetails, workdayManpowerInterfaceDetail);
		if (message != null && message.equals("SUCCESS")) {
			retriggerWorkdayManpowerInterfaceDetails(award, workdayManpowerInterfaceDetail, null);
		}
		vo.setWorkdayInterfaceLogDtos(manpowerIntegrationDao.getWorkdayInterfaceLogDtosByWorkdayManpowerInterfaceId(vo.getWorkdayManpowerInterfaceId()));
		workdayManpowerInterface.setManpowerInterfaceStatus(manpowerIntegrationDao.fetchManpowerInterfaceStatusById(Constants.MANPOWER_INTERFACE_RETRIGGERED));
		workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_RETRIGGERED);
		workdayManpowerInterface.setComments(vo.getComments());
		workdayManpowerInterface.setManpowerUserActionCode(vo.getManpowerUserActionCode());
		workdayManpowerInterface.setUpdateTimestamp(commonDao.getCurrentTimestamp());
        workdayManpowerInterface.setUpdateUser(AuthenticatedUser.getLoginUserName());
        manpowerIntegrationDao.saveOrUpdateManpowerInterface(workdayManpowerInterface);
		return manpowerIntegrationService.getAwardTriggerDetails(vo);
	}

	private void retriggerWorkdayManpowerInterfaceDetails(Award award, WorkdayManpowerInterface workdayManpowerInterfaceDetail, String resourceUniqueId) {
		List<String> interfaceTypeCodes = new ArrayList<>();
		try {
			if (resourceUniqueId == null) {
				interfaceTypeCodes.add(Constants.MANPOWER_INTERFACE_POSITION_CREATION);
				interfaceTypeCodes.add(Constants.MANPOWER_INTERFACE_MOVE_WORKERS);
			}
			interfaceTypeCodes.add(Constants.MANPOWER_INTERFACE_HRBP);
			interfaceTypeCodes.add(Constants.MANPOWER_INTERFACE_FREEZE_POSITION);
			List<WorkdayManpowerInterface> workdayManpowerInterfaces = manpowerIntegrationDao.fetchManpowerInterfacesByAwardNumberAndInterfaceStatusAndInterfaceTypeCodesAndResourceUniqueId(workdayManpowerInterfaceDetail.getAwardNumber(), Constants.MANPOWER_INTERFACE_ERROR, interfaceTypeCodes, resourceUniqueId);
			StringBuilder newSuperiorSupOrg = new StringBuilder("");
			String superiorSupOrg = null;
			superiorSupOrg = manpowerService.findSuperiorSupOrgForAward(award);
			if (superiorSupOrg != null && !superiorSupOrg.equals("")) {
				newSuperiorSupOrg.append(superiorSupOrg);
			}
			for (WorkdayManpowerInterface manpowerInterface : workdayManpowerInterfaces) {
				try {
					List<WorkdayManpowerInterface> manpowerInterfaces = new ArrayList<>();
					WorkdayManpowerInterface manpowerInterfaceDetail = new WorkdayManpowerInterface();
					ReflectionUtils.shallowCopyFieldState(manpowerInterface, manpowerInterfaceDetail);
					manpowerInterfaceDetail.setWorkdayManpowerInterfaceId(null);
					manpowerInterfaceDetail = saveWorkdayManpowerInterface(manpowerInterfaceDetail);
					manpowerInterfaces.add(manpowerInterfaceDetail);
					workdayManpowerInterface(award, manpowerInterfaces, newSuperiorSupOrg, Boolean.TRUE);
					manpowerInterface.setManpowerInterfaceStatus(manpowerIntegrationDao.fetchManpowerInterfaceStatusById(Constants.MANPOWER_INTERFACE_FEED_NOT_NEEDED));
					manpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_FEED_NOT_NEEDED);
					manpowerInterface.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					manpowerInterface.setUpdateUser(AuthenticatedUser.getLoginUserName());
			        manpowerIntegrationDao.saveOrUpdateManpowerInterface(manpowerInterface);
				} catch (Exception e) {
					logger.error("Exception in retriggerWorkdayManpowerInterfaceDetails {} ", e.getMessage());
					e.printStackTrace();
				}
			}
		} catch (Exception e) {
			logger.error("Error in retriggerWorkdayManpowerInterfaceDetails {} ", e.getMessage());
			e.printStackTrace();
		}
	}

	private WorkdayManpowerInterface saveWorkdayManpowerInterface(WorkdayManpowerInterface workdayManpowerInterface) {
		workdayManpowerInterface.setCreateTimestamp(commonDao.getCurrentTimestamp());
		workdayManpowerInterface.setCreateUser(AuthenticatedUser.getLoginUserName());
		workdayManpowerInterface.setUpdateTimestamp(commonDao.getCurrentTimestamp());
        workdayManpowerInterface.setUpdateUser(AuthenticatedUser.getLoginUserName());
        workdayManpowerInterface.setInterfaceTimestamp(commonDao.getCurrentTimestamp());
		return manpowerIntegrationDao.saveOrUpdateManpowerInterface(workdayManpowerInterface);
	}

	@Override
	public String retriggerWorkdayApi(ManpowerIntegrationVO vo) {
		WorkdayManpowerInterface workdayManpowerInterfaceDetail = new WorkdayManpowerInterface();
		try {
			WorkdayManpowerInterface workdayManpowerInterface = manpowerIntegrationDao.getWorkdayManpowerInterfaceById(vo.getWorkdayManpowerInterfaceId());
			ReflectionUtils.shallowCopyFieldState(workdayManpowerInterface, workdayManpowerInterfaceDetail);
			workdayManpowerInterfaceDetail.setWorkdayManpowerInterfaceId(null);
			workdayManpowerInterfaceDetail = saveWorkdayManpowerInterface(workdayManpowerInterfaceDetail);
			vo.setResourceUniqueId(workdayManpowerInterfaceDetail.getResourceUniqueId());
			Award award = awardDao.fetchActiveAwardByAwardNumber(workdayManpowerInterface.getAwardNumber());
			StringBuilder newSuperiorSupOrg = new StringBuilder("");
			String superiorSupOrg = null;
			superiorSupOrg = manpowerService.findSuperiorSupOrgForAward(award);
			if (superiorSupOrg != null && !superiorSupOrg.equals("")) {
				newSuperiorSupOrg.append(superiorSupOrg);
			}
			List<WorkdayManpowerInterface> workdayManpowerInterfaces = new ArrayList<>();
			workdayManpowerInterfaces.add(workdayManpowerInterfaceDetail);
			workdayManpowerInterface.setManpowerInterfaceStatus(manpowerIntegrationDao.fetchManpowerInterfaceStatusById(Constants.MANPOWER_INTERFACE_RETRIGGERED));
			workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_RETRIGGERED);
			workdayManpowerInterface.setComments(vo.getComments());
			workdayManpowerInterface.setManpowerUserActionCode(vo.getManpowerUserActionCode());
			workdayManpowerInterface.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			workdayManpowerInterface.setUpdateUser(AuthenticatedUser.getLoginUserName());
			manpowerIntegrationDao.saveOrUpdateManpowerInterface(workdayManpowerInterface);
			if (workdayManpowerInterfaceDetail.getInterfaceTypeCode().equals(Constants.MANPOWER_INTERFACE_COST_ALLOCATION_AFTER_MANPOWER)) {
				assignCostAllocationDetails(workdayManpowerInterfaces);
			} else {
				workdayManpowerInterface(award, workdayManpowerInterfaces, newSuperiorSupOrg, Boolean.TRUE);
			}
			if (workdayManpowerInterface.getInterfaceTypeCode().equals(Constants.MANPOWER_INTERFACE_POSITION_CREATION)) {
				retriggerWorkdayManpowerInterfaceDetails(award, workdayManpowerInterfaceDetail, workdayManpowerInterface.getResourceUniqueId());
			}
			vo.setParentInterfaceStatus(manpowerIntegrationDao.getInterfaceErrorCountByResourceUniqueId(workdayManpowerInterface.getResourceUniqueId()) > 0 ? "Error" : "Retriggered" );
		} catch (Exception e) {
			logger.error("Error in retriggerWorkdayApi {} ", e.getMessage());
		}
		return manpowerIntegrationService.getPositionErrorDetails(vo);
	}

	@Scheduled(cron = "${workday.costAllocation.notification.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void sendNotificationForLessCostAllocation() {
		logger.info("In sendNotificationForLessCostAllocation {}");
		manpowerIntegrationService.sendNotificationForLessCostAllocation();
	}

	@Override
	public void threadForFetchAndIntegrateManpower(String awardNumber) {
		if (!executorService.isTerminated()) {
			executorService.execute(() -> integrateManpower(awardNumber));
		}
	}

	@Override
	public void integrateManpower(String awardNumber) {
		logger.info("integrateManpower");
		try {
			Award activeAward = awardDao.fetchActiveAwardByAwardNumber(awardNumber);
			if (activeAward != null) {
				StringBuilder newSuperiorSupOrg = new StringBuilder("");
				String superiorSupOrg = null;
				superiorSupOrg = manpowerService.findSuperiorSupOrgForAward(activeAward);
				if (superiorSupOrg != null && !superiorSupOrg.equals("")) {
					newSuperiorSupOrg.append(superiorSupOrg);
				}
				workdayManpowerInterface(activeAward, null, newSuperiorSupOrg, Boolean.FALSE);
			}
		} catch (Exception e) {
			logger.error("Error in threadForFetchAndIntegrateManpower {} ", e.getMessage());
		}
	}

	@Override
	public String retriggerWorkdayClosePosition(ManpowerIntegrationVO vo) {
		try {
			WorkdayManpowerInterface workdayManpowerInterface = manpowerIntegrationDao.getWorkdayManpowerInterfaceById(vo.getWorkdayManpowerInterfaceId());
			workdayManpowerInterface.setManpowerInterfaceStatus(manpowerIntegrationDao.fetchManpowerInterfaceStatusById(Constants.MANPOWER_INTERFACE_RETRIGGERED));
			workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_RETRIGGERED);
			workdayManpowerInterface.setComments(vo.getComments());
			workdayManpowerInterface.setManpowerUserActionCode(vo.getManpowerUserActionCode());
			workdayManpowerInterface.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			workdayManpowerInterface.setUpdateUser(AuthenticatedUser.getLoginUserName());
			vo.setResourceUniqueId(workdayManpowerInterface.getResourceUniqueId());
			manpowerIntegrationDao.saveOrUpdateManpowerInterface(workdayManpowerInterface);
			Award award = awardDao.fetchActiveAwardByAwardNumber(workdayManpowerInterface.getAwardNumber());
			AwardClosePositionResorceDto closePositionManpowerResource = new AwardClosePositionResorceDto(workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getAwardNumber(), workdayManpowerInterface.getAwardManpowerResource().getPositionId(), "SINGLE", manpowerIntegrationDao.getCampusByUnit(award.getLeadUnitNumber()));
			List<AwardClosePositionResorceDto> closePositionManpowerResources = new ArrayList<>();
			closePositionManpowerResources.add(closePositionManpowerResource);
			closePositionResources(closePositionManpowerResources);
		} catch(Exception e) {
			logger.error("Error in retriggerWorkdayClosePosition {} ", e.getMessage());
		}
		return manpowerIntegrationService.getPositionErrorDetails(vo);
	}

}
