package com.polus.fibicomp.fastintegration.sapfeedmaintenance.service;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardDocumentType;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.SftpConfigurationService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.fastintegration.pojo.SapAwardFeed;
import com.polus.fibicomp.fastintegration.pojo.SapFeedUserAction;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.dao.SapFeedMaintenanceDao;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.dto.SapFeedMaintenanceDto;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;
import com.polus.fibicomp.fastintegration.service.FastIntegrationService;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.notification.render.AwardRenderService;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestType;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "sapFeedMaintenanceService")
public class SapFeedMaintenanceServiceImpl implements SapFeedMaintenanceService {

	protected static Logger logger = LogManager.getLogger(SapFeedMaintenanceServiceImpl.class.getName());

	@Autowired
	private SapFeedMaintenanceDao sapFeedMaintenanceDao;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private EmailService emailService;

	@Autowired
	private AwardRenderService awardRenderService;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	FastIntegrationService fastIntegrationService;

	@Autowired
	AwardBudgetDao awardBudgetDao;

	@Autowired
	public PersonDao personDao;

	@Autowired
	private SftpConfigurationService sftpConfigurationService;

	@Autowired
	private AwardService awardService;

	@Autowired
	public PrintService printService;

	@Autowired
	private DashboardService dashboardService;

	private static final String ALL = "L";
    private static final String VIEW_SAP_FEED_RIGHT_NAME = "VIEW_SAP_FEED";
    private static final String SUCCESS = "Success";
    private static final String FAILED = "Failed";

	@Override
	public String getBatchDetailDashboard(SapFeedMaintenanceVO vo) {
		return commonDao.convertObjectToJSON(prepareBatchDetails(vo));
	}

	private SapFeedMaintenanceVO prepareBatchDetails(SapFeedMaintenanceVO vo) {
		Integer batchId = null;
		List<String> status = new ArrayList<>();
		if (vo.getTabName().equals(Constants.BATCH_DETAIL) && vo.getIsAdvanceSearch().equals(ALL)) {
			batchId = sapFeedMaintenanceDao.getLatesSapAwardFeedBatchId();
			vo.setProperty1(batchId);
			status.add(Constants.SAP_FEED_STATUS_ERROR);
			vo.setProperty4(status);
		} else if(vo.getTabName().equals(Constants.PENDING_FEED) && vo.getIsAdvanceSearch().equals(ALL)) {
			status.add(Constants.SAP_FEED_STATUS_PENDING);
			vo.setProperty4(status);
		}
		List<SapAwardFeed> sapAwardFeedDetail = prepareSapAwardFeedDetail(vo).getSapAwardFeeds();
		if (Boolean.FALSE.equals(vo.getIsDownload())) {
			if (vo.getProperty1() != null) {
				vo.getProperty4().clear();
				prepareSapFeedHistory(vo, vo.getProperty1());
			} else {
				prepareSapFeedHistory(vo, batchId);
				if (vo.getSapAwardFeedHistory() == null) {
					setSapHistoryByStatus(Constants.SAP_FEED_STATUS_PENDING, vo, batchId);
				}
				if (vo.getSapAwardFeedHistory() == null) {
					setSapHistoryByStatus(Constants.SAP_FEED_STATUS_CANCELLED, vo, batchId);
				}
			}
			vo.setSapFeedUserActions(sapFeedMaintenanceDao.getSapFeedUserAction());
		}
		vo.setSapAwardFeeds(sapAwardFeedDetail);
		return vo;
	}

	private SapFeedMaintenanceVO setSapHistoryByStatus(String feedStatus, SapFeedMaintenanceVO vo, Integer batchId) {
		List<String> status = new ArrayList<>();
		vo.getProperty4().clear();
		status.add(feedStatus);
		vo.setProperty4(status);
		prepareSapFeedHistory(vo, batchId);
		return vo;
	}

	private SapFeedMaintenanceVO prepareSapAwardFeedDetail(SapFeedMaintenanceVO vo) {
		vo.setSapAwardFeeds(sapFeedMaintenanceDao.getSapFeedMaintenanceBatchDetail(vo));
		vo.setTotalCount(sapFeedMaintenanceDao.getSapFeedMaintenanceBatchDetailCount(vo));
		return vo;
	}

	private SapAwardFeed setAwardFeedDetails(SapAwardFeed sapAwardFeed) {
		AwardDocumentType awardDocumentType = awardDao.getAwardDocumentTypeByAwardId(sapAwardFeed.getAwardId());
		if (awardDocumentType != null && awardDocumentType.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP)) {
			sapAwardFeed.setAwardDocumentType(awardDocumentType.getDescription());
		}
		if (sapAwardFeed.getAwardDocumentType() == null) {
			ServiceRequestType serviceRequestType = sapFeedMaintenanceDao.getServiceRequestTypeByAwardId(sapAwardFeed.getAwardId());
			if (serviceRequestType != null) {
				sapAwardFeed.setServiceRequestType(serviceRequestType);
			}
		}
		return sapAwardFeed;
	}

	private SapFeedMaintenanceVO prepareSapFeedHistory(SapFeedMaintenanceVO vo, Integer batchId) {
		if ((vo.getIsAdvanceSearch().equals("A") && vo.getProperty1() != null) || vo.getIsAdvanceSearch().equals(ALL) && batchId != null) {
			SapFeedMaintenanceVO sapFeedMaintenanceVO = new SapFeedMaintenanceVO();
			sapFeedMaintenanceVO.setProperty1(batchId);
			sapFeedMaintenanceVO.setProperty4(vo.getProperty4());
			List<SapAwardFeed> sapAwardFeedDetails = sapFeedMaintenanceDao.getBatchHistoryDetail(sapFeedMaintenanceVO);
			if (sapAwardFeedDetails != null && !sapAwardFeedDetails.isEmpty()) {
				vo.setSapAwardFeedHistory(sapAwardFeedDetails.get(0));
			}
		}
		return vo;
	}

	@Override
	public String fetchSapAwardFeedStatus() {
		SapFeedMaintenanceVO sapFeedMaintenanceVO = new SapFeedMaintenanceVO();
		sapFeedMaintenanceVO.setSapFeedStatus(sapFeedMaintenanceDao.getSapAwardFeedStatus());
		return commonDao.convertObjectToJSON(sapFeedMaintenanceVO);
	}

	@Override
	public String getSapAwardFeedDetails(SapFeedMaintenanceVO vo) {
		SapFeedMaintenanceDto sapFeedMaintenanceDto = new SapFeedMaintenanceDto();
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		Boolean viewSapFeedRight = Boolean.FALSE;
		String leadUnitNumber = awardDao.fetchAwardLeadUnitNumberByAwardId(vo.getAwardId());
		if (leadUnitNumber != null) {
			viewSapFeedRight = personDao.isPersonHasPermission(vo.getPersonId(), VIEW_SAP_FEED_RIGHT_NAME, leadUnitNumber);
		}
		Boolean viewRightFromPerimmision = awardDao.checkPersonHasRightInAward(vo.getPersonId(), VIEW_SAP_FEED_RIGHT_NAME, vo.getAwardId());
		if ((viewSapFeedRight != null && Boolean.TRUE.equals(viewSapFeedRight)) || (viewRightFromPerimmision != null && Boolean.TRUE.equals(viewRightFromPerimmision))) {
			vo.setSapFeedMaintenanceDto(prepareSapFeedInterfaceDetails(sapFeedMaintenanceDto, vo.getFeedId()));
			vo.setIsViewSapFeedRightExist(Boolean.TRUE);
		}
		return commonDao.convertObjectToJSON(vo);
	}
 
	private SapFeedMaintenanceDto prepareSapFeedInterfaceDetails(SapFeedMaintenanceDto sapFeedMaintenanceDto, Integer feedId) {
		sapFeedMaintenanceDto.setSapFeedTmplProjectDefs(sapFeedMaintenanceDao.getSapFeedTmplProjectDefByFeedId(feedId));
		sapFeedMaintenanceDto.setSapFeedTmplGrantBudMasters(sapFeedMaintenanceDao.getSapFeedTmplGrantBudMasterByFeedId(feedId));
		sapFeedMaintenanceDto.setSapFeedTmplFundedPrgms(sapFeedMaintenanceDao.getSapFeedTmplFundedPrgmByFeedId(feedId));
		sapFeedMaintenanceDto.setSapFeedTmplFmBudgets(sapFeedMaintenanceDao.getSapFeedTmplFmBudgetbyFeedId(feedId));
		sapFeedMaintenanceDto.setSapFeedTmplSponsoPrgms(sapFeedMaintenanceDao.getSapFeedTmplSponsoPrgmByFeedId(feedId));
		sapFeedMaintenanceDto.setSapFeedTmplSponsorClasses(sapFeedMaintenanceDao.getSapFeedTmplSponsorClasseByFeedId(feedId));
		sapFeedMaintenanceDto.setSapFeedTmplWbs(sapFeedMaintenanceDao.getSapFeedTmplWbsByFeedId(feedId));
		sapFeedMaintenanceDto.setSapFeedTmplGrantMasters(sapFeedMaintenanceDao.getSapFeedTmplGrantMasterByFeedId(feedId));
		return sapFeedMaintenanceDto;
	}

	@Override
	public String getBatchHistoryDashboard(SapFeedMaintenanceVO vo) {
		if (vo.getIsAdvanceSearch().equals(ALL)) {
			vo.setProperty4(new ArrayList<>());
		}
		if (vo.getProperty4().isEmpty()) {
			vo.setProperty4(new ArrayList<>());
		}
		vo.setSapAwardFeeds(sapFeedMaintenanceDao.getBatchHistoryDetail(vo));
		vo.setSapFeedUserActions(sapFeedMaintenanceDao.getSapFeedUserAction());
		vo.setTotalCount(sapFeedMaintenanceDao.getBatchHistoryDetailCount(vo));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String updateFeedStatus(SapFeedMaintenanceVO vo) {
		try {
		List<Integer> feedIds = new ArrayList<>();
		List<SapAwardFeed> sapAwardFeedDetails = vo.getSapAwardFeeds();
		sapAwardFeedDetails.stream().forEach(sapAwardFeedDetail -> {
			if (sapAwardFeedDetail.getFeedId() != null && Boolean.TRUE.equals(isPersonHasRight(sapAwardFeedDetail.getAwardNumber()))) {
				feedIds.add(sapAwardFeedDetail.getFeedId());
			} else {
				vo.getFailedAwardNumbers().add(sapAwardFeedDetail.getAwardNumber());
			}
		});
		if(!feedIds.isEmpty() && vo.getChangeType().equals(Constants.UPDATE_FEED_STATUS_TAB)) {
			sapFeedMaintenanceDao.updateSapAwardFeedById(vo.getUserComment(), getUserActionDetails(vo.getSapFeedUserAction()), feedIds);
			sapFeedMaintenanceDao.updateSponsorClass(feedIds, vo.getUserComment());
			sapFeedMaintenanceDao.updateSponsoPrgm(feedIds, vo.getUserComment());
			sapFeedMaintenanceDao.updateFundedPrgm(feedIds, vo.getUserComment());
			sapFeedMaintenanceDao.updateGrantMaster(feedIds, vo.getUserComment());
			sapFeedMaintenanceDao.updateProjectDef(feedIds, vo.getUserComment());
			sapFeedMaintenanceDao.updateGrantBudMaster(feedIds, vo.getUserComment());
			sapFeedMaintenanceDao.updateWBS(feedIds, vo.getUserComment());
			sapFeedMaintenanceDao.updateFmBudgets(feedIds, vo.getUserComment());
			prepareSapAwardFeed(feedIds, vo);
		} else {
			for (Integer feedId : feedIds) {
				List<String> feedStatuses = new ArrayList<>();
				if (vo.getChangeType().equals(Constants.CANCEL_FEED_TAB)) {
					feedStatuses.add(Constants.SAP_FEED_STATUS_PENDING);
					feedStatuses.add(Constants.SAP_FEED_STATUS_CONCURRENT);
					List<SapAwardFeed> sapAwardFeeds = sapFeedMaintenanceDao.getAllConcurrentFeedIds(feedId, feedStatuses);
					if (sapAwardFeeds != null && !sapAwardFeeds.isEmpty()) {
						String awardNumber = sapAwardFeeds.get(0).getAwardNumber();
						Set<Integer> awardIds = sapAwardFeeds.stream().map(SapAwardFeed::getAwardId).collect(Collectors.toSet());
						List<Integer> versionNumbers = awardBudgetDao.getAwardBudgetVersionBasedOnAwardIdsAndStatus(awardIds, awardNumber, Constants.AWARD_BUDGET_STATUS_CODE_ACTIVE);
						if (versionNumbers != null && !versionNumbers.isEmpty()) {
							awardBudgetDao.updateAwardBudgetHeadersBasedOnVersionNumbers(versionNumbers, Constants.AWARD_BUDGET_STATUS_CODE_ERROR_IN_POSTING, awardNumber, Constants.AWARD_BUDGET_STATUS_CODE_ACTIVE);
						}
						updateSapFeed(Constants.SAP_FEED_STATUS_PENDING, Constants.SAP_FEED_STATUS_CANCELLED, vo.getUserComment(), getUserActionDetails(vo.getSapFeedUserAction()), sapAwardFeeds);
					}
				} else if (vo.getChangeType().equals(Constants.HOLD_FEED)) {
					feedStatuses.add(Constants.SAP_FEED_STATUS_PENDING);
					feedStatuses.add(Constants.SAP_FEED_STATUS_CONCURRENT);
					List<SapAwardFeed> sapAwardFeeds = sapFeedMaintenanceDao.getAllConcurrentFeedIds(feedId, feedStatuses);
					if (sapAwardFeeds != null && !sapAwardFeeds.isEmpty()) {
						updateSapFeed(Constants.SAP_FEED_STATUS_PENDING, Constants.SAP_FEED_STATUS_HOLD, vo.getUserComment(), getUserActionDetails(vo.getSapFeedUserAction()), sapAwardFeeds);
					}
				} else if (vo.getChangeType().equals(Constants.QUE_TO_FEED_TAB)) {
					feedStatuses.add(Constants.SAP_FEED_STATUS_HOLD);
					feedStatuses.add(Constants.SAP_FEED_STATUS_CONCURRENT_HOLD);
					List<SapAwardFeed> sapAwardFeeds = sapFeedMaintenanceDao.getAllConcurrentFeedIds(feedId, feedStatuses);
					if (sapAwardFeeds != null && !sapAwardFeeds.isEmpty()) {
						updateSapFeed(Constants.SAP_FEED_STATUS_HOLD, Constants.SAP_FEED_STATUS_PENDING, vo.getUserComment(), getUserActionDetails(vo.getSapFeedUserAction()), sapAwardFeeds);
					}
				}
			}
		}
		vo.setMessage(SUCCESS);
		getBatchDetailDashboard(vo);
		return commonDao.convertObjectToJSON(vo);
		} catch(Exception e) {
			vo.setMessage(FAILED);
			return commonDao.convertObjectToJSON(vo);
		}
	}

	private void updateSapFeed(String currentStatus, String newStatus, String userComment, String userActionDetails, List<SapAwardFeed> sapAwardFeeds) {
		List<Integer> concurrentFeedIds = new ArrayList<>();
		List<Integer> pendingFeedIds = new ArrayList<>();
		List<Integer> holdFeedIds = new ArrayList<>();
		for (SapAwardFeed sapAwardFeed : sapAwardFeeds) {
			if (sapAwardFeed.getFeedStatus().equals(Constants.SAP_FEED_STATUS_PENDING)) {
				pendingFeedIds.add(sapAwardFeed.getFeedId());
			} else if (sapAwardFeed.getFeedStatus().equals(Constants.SAP_FEED_STATUS_CONCURRENT) || (sapAwardFeed.getFeedStatus().equals(Constants.SAP_FEED_STATUS_CONCURRENT_CANCELLED))
					|| (sapAwardFeed.getFeedStatus().equals(Constants.SAP_FEED_STATUS_CONCURRENT_HOLD))) {
				concurrentFeedIds.add(sapAwardFeed.getFeedId());
			} else if (sapAwardFeed.getFeedStatus().equals(Constants.SAP_FEED_STATUS_HOLD)) {
				holdFeedIds.add(sapAwardFeed.getFeedId());
			}
		}
		if (newStatus.equals(Constants.SAP_FEED_STATUS_CANCELLED)) {
			if (!pendingFeedIds.isEmpty()) {
				updateSapAwardFeed(currentStatus, newStatus, userComment, userActionDetails,  pendingFeedIds);
			}
			if (!concurrentFeedIds.isEmpty()) {
				updateSapAwardFeed(Constants.SAP_FEED_STATUS_CONCURRENT, Constants.SAP_FEED_STATUS_CONCURRENT_CANCELLED, userComment, userActionDetails,  concurrentFeedIds);
			}
		} else if (newStatus.equals(Constants.SAP_FEED_STATUS_HOLD)) {
			if (!pendingFeedIds.isEmpty()) {
				updateSapAwardFeed(currentStatus, newStatus, userComment, userActionDetails,  pendingFeedIds);
			}
			if (!concurrentFeedIds.isEmpty()) {
				updateSapAwardFeed(Constants.SAP_FEED_STATUS_CONCURRENT, Constants.SAP_FEED_STATUS_CONCURRENT_HOLD, userComment, userActionDetails,  concurrentFeedIds);
			}
		} else if (newStatus.equals(Constants.SAP_FEED_STATUS_PENDING)) {
			if (!holdFeedIds.isEmpty()) {
				updateSapAwardFeed(currentStatus, newStatus, userComment, userActionDetails,  holdFeedIds);
			}
			if (!concurrentFeedIds.isEmpty()) {
				updateSapAwardFeed(Constants.SAP_FEED_STATUS_CONCURRENT_HOLD, Constants.SAP_FEED_STATUS_CONCURRENT, userComment, userActionDetails,  concurrentFeedIds);
			}
		}
		
	}

	private String getUserActionDetails(SapFeedUserAction sapFeedUserAction) {
		return sapFeedUserAction!= null && sapFeedUserAction.getUserActionCode() != null ? sapFeedUserAction.getUserActionCode() : null;
	}

	private void prepareSapAwardFeed(List<Integer> feedIds, SapFeedMaintenanceVO vo) {
		List<SapAwardFeed> sapAwardFeedDatas = new ArrayList<>();
		List<SapAwardFeed> sapAwardFeeds = sapFeedMaintenanceDao.getSapAwardFeeds(feedIds);
		sapAwardFeeds.stream().forEach(sapAwardFeed -> {
			updateAwardBudgetHeaderStatus(sapAwardFeed);
			sapAwardFeedDatas.add(sapAwardFeed);
		});
		vo.setSapAwardFeeds(sapAwardFeedDatas);
	}

	private void updateAwardBudgetHeaderStatus(SapAwardFeed sapAwardFeed) {
		awardService.updateAwardBudgetStatusBasedOnBatchId(sapAwardFeed.getBatchId(), sapAwardFeed.getAwardNumber(),Constants.AWARD_BUDGET_STATUS_CODE_POSTED, Constants.AWARD_BUDGET_STATUS_CODE_ERROR_IN_POSTING);
	}

	private void updateSapAwardFeed(String status, String changeStatus, String userComment, String userActionCode, List<Integer> feedIds) {
		sapFeedMaintenanceDao.updateSapAwardFeedByStatus(status, changeStatus, userComment, userActionCode, feedIds);
	}

	@Override
	public String reInterfaceSapAwardFeed(SapFeedMaintenanceVO vo) {
	    try {
		List<Integer> feedIds = new ArrayList<>();
		List<SapAwardFeed> sapAwardFeeds = vo.getSapAwardFeeds();
		sapAwardFeeds.stream().forEach(sapAwardFeed -> {
			if (Boolean.TRUE.equals(isPersonHasRight(sapAwardFeed.getAwardNumber()))) {
				sapFeedMaintenanceDao.updateSapAwardFeedUserAction(getUserActionDetails(vo.getSapFeedUserAction()), vo.getUserComment(), sapAwardFeed.getFeedId());
				Integer feedId = sapFeedMaintenanceDao.reInterfaceSapAwardFeed(sapAwardFeed.getAwardId(), sapAwardFeed.getAwardNumber(),sapAwardFeed.getSequenceNumber(), sapAwardFeed.getUpdateUser(), sapAwardFeed.getFeedType());
				feedIds.add(feedId);
				vo.setAwardId(sapAwardFeed.getAwardId());
				sendSapFeedNotification(vo, Constants.SAP_FEED_REINTERFACE_NOTIFICATION_CODE, new HashSet<>());
			} else {
				vo.getFailedAwardNumbers().add(sapAwardFeed.getAwardNumber());
			}
		});
		if (!feedIds.isEmpty()) {
			vo.setSapAwardFeeds(sapFeedMaintenanceDao.getSapAwardFeeds(feedIds));
		}
		vo.setMessage(SUCCESS);
		getBatchDetailDashboard(vo);
		return commonDao.convertObjectToJSON(vo);
	    } catch (Exception e) {
	    	vo.setMessage(FAILED);
	    	return commonDao.convertObjectToJSON(vo);
	    }
	}

	@Override
	public String exportSapGeneratedAttachments(SapFeedMaintenanceVO vo, HttpServletResponse response) {
		StringBuilder fileName = new StringBuilder();
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		ZipOutputStream zos = new ZipOutputStream(baos);
		Integer batchId = vo.getBatchId();
		fileName.append(batchId.toString()).append("_").append("attachments");
		response.setContentType("application/zip");
		response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + ".zip" + "\"");
		File[] attachments = getSapAwardFeedFiles(batchId, sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_FAST_INBOUND));
		try {
			byte[] buf = new byte[1024];
			if (attachments != null && attachments.length != 0) {
				Integer index = 0;
				for (File attachment : attachments) {
					int length;
					FileInputStream in = new FileInputStream(attachment);
					index = commonService.addFilesToZipFolder(index, attachment.getName(), zos);
					while ((length = in.read(buf)) > 0) {
						zos.write(buf, 0, length);
					}
					in.close();
				}
			} else {
				return "Empty_Zip";
			}
			zos.closeEntry();
			zos.flush();
			baos.flush();
			zos.close();
			baos.close();
			ServletOutputStream op = response.getOutputStream();
			op.write(baos.toByteArray());
			op.flush();
			op.close();
		} catch (Exception e) {
			logger.error("error in exportSapGeneratedAttachments :{}", e.getMessage());
		} finally {
			try {
				zos.close();
				baos.close();
			} catch (IOException e) {
				logger.error("error in exportSapGeneratedAttachments :{}", e.getMessage());
			}
		}
		return commonDao.convertObjectToJSON("File Exists");
	}

	@Override
	public File[] getSapAwardFeedFiles(Integer batchId, String inbound) {
		File directory = new File(inbound);
		FilenameFilter textFilefilter = new FilenameFilter() {
			public boolean accept(File dir, String name) {
				String lowercaseName = name.toLowerCase();
				if (lowercaseName.contains("_" + batchId + "_")) {
					return true;
				} else {
					return false;
				}
			}
		};
		return directory.listFiles(textFilefilter);
	}

	@Override
	public String notifyPI(SapFeedMaintenanceVO vo) {
		try {
			List<SapAwardFeed> sapAwardFeeds = vo.getSapAwardFeeds();
			sapAwardFeeds.stream().forEach(sapAwardFeed -> {
				if (Boolean.TRUE.equals(isPersonHasRight(sapAwardFeed.getAwardNumber()))) {
					vo.setAwardId(sapAwardFeed.getAwardId());
					sapFeedMaintenanceDao.updateSapAwardFeedUserAction(getUserActionDetails(vo.getSapFeedUserAction()),
							vo.getUserComment(), sapAwardFeed.getFeedId());
					sendSapFeedNotification(vo, Constants.SAP_FEED_FAILED_NOTIFICATION_CODE, new HashSet<>());
				} else {
					vo.getFailedAwardNumbers().add(sapAwardFeed.getAwardNumber());
				}
			});
			vo.setMessage(SUCCESS);
			getBatchDetailDashboard(vo);
			return commonDao.convertObjectToJSON(vo);
		} catch (Exception e) {
			vo.setMessage(FAILED);
			return commonDao.convertObjectToJSON(vo);
		}
	}

	private SapFeedMaintenanceVO sendSapFeedNotification(SapFeedMaintenanceVO vo, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setModuleItemKey(vo.getAwardId().toString());
		emailServiceVO.setPlaceHolder(getSapFeedPlaceholders(vo));
		emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailService.sendEmail(emailServiceVO);
		return vo;
	}

	private Map<String, String> getSapFeedPlaceholders(SapFeedMaintenanceVO vo) {
		Award award = awardDao.fetchAwardByAwardId(vo.getAwardId().toString());
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{USER_COMMENT}", vo.getUserComment());
		placeHolder.put("{USER_ACTION}", vo.getUserAction());
		if (award != null) {
			Map<String, String> awardPlaceHolder = awardRenderService.getAwardPlaceHolder(award);
			placeHolder.putAll(awardPlaceHolder);
		}
		return placeHolder;
	}

	@Override
	public String sapFeedReTrigger(SapFeedMaintenanceVO vo) {
		try {
			File[] attachments = getSapAwardFeedFiles(vo.getBatchId(), sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_FAST_INBOUND));
			sapFeedFileMove(attachments);
			IntegrationReportVO integrationReportVO = new IntegrationReportVO();
			fastIntegrationService.prepareSapAwardFeedFiles(integrationReportVO, vo.getBatchId());
			vo.setMessage(SUCCESS);
			return commonDao.convertObjectToJSON(vo);
	    } catch (Exception e) {
	    	vo.setMessage(FAILED);
	    	logger.error("error in sapFeedReTrigger :{}", e.getMessage());
	    	return commonDao.convertObjectToJSON(vo);
	    }
	}

	private void sapFeedFileMove(File[] files) {
		try {
			if (files.length != 0)
				for( File file : files) {
					StringBuilder path = new StringBuilder(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SAP_FEED_INBOUND_ARCHIVE));
					path.append("/");
					path.append(file.getName());
					Path temp = Files.move(Paths.get(file.getPath()), Paths.get(path.toString()),
							StandardCopyOption.REPLACE_EXISTING);
					if (temp != null)
						logger.info("file successfully moved from {} -> {} ", file.getPath(), temp);
				}
		}catch (Exception e){
			logger.error("error in sap feed inbound file move :{}", e.getMessage());
		}
	}

	private Boolean isPersonHasRight(String awardNumber) {
		String leadUnitNumber = awardDao.fetchActiveAwardLeadUnitByAwardNumber(awardNumber);
		logger.info(leadUnitNumber);
		if (leadUnitNumber != null) {
			return personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), Constants.MAINTAIN_SAP_FEED, leadUnitNumber);
		} else {
			return Boolean.FALSE;
		}
	}

	@Override
	public ResponseEntity<byte[]> generateSapFeedReport(HttpServletResponse response, SapFeedMaintenanceVO vo) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			vo.setIsDownload(Boolean.TRUE);
			vo = prepareBatchDetails(vo);
			attachmentData = generateSapFeedExcelReport(vo.getSapAwardFeeds(), attachmentData, vo);
		} catch (Exception e) {
			logger.error("Exception in generateSapFeedReport : {} ", e.getMessage());
		}
		return attachmentData;
	}

	private ResponseEntity<byte[]> generateSapFeedExcelReport(List<SapAwardFeed> sapAwardFeeds,
			ResponseEntity<byte[]> attachmentData, SapFeedMaintenanceVO vo) {
		XSSFWorkbook workbook = new XSSFWorkbook();
		try {
			CommonVO commonVo = new CommonVO();
			commonVo.setExportType("xlsx");
			commonVo.setDocumentHeading(vo.getDocumentHeading());   
			XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
	        commonService.addDetailsInHeader(workbook, sheet);
	        prepareExcelSheetForSapFeedReport(sapAwardFeeds, sheet, workbook, vo.getTabName(), vo.getDocumentHeading());
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			attachmentData = dashboardService.getResponseEntityForDownload(commonVo, workbook);
			workbook.close();
	        } catch (Exception e) {
				logger.error("error in generateSapFeedExcelReport {}", e.getMessage());
			} finally {
				try {
					workbook.close();
				} catch (IOException e) {
					logger.error("error in generateSapFeedExcelReport {}", e.getMessage());
				}
			}
			return attachmentData;
	}

	private void prepareExcelSheetForSapFeedReport(List<SapAwardFeed> sapAwardFeeds, XSSFSheet sheet,
			XSSFWorkbook workbook, String tabName, String documentHeading) {
		try {
			XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
	        sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 4));
			int rowNumber = 0;
			int totalColumnCount = 7;
			rowNumber = rowNumber + 1;
			printService.prepareExcelSheetHeading(sheet, documentHeading, workbook, totalColumnCount, rowNumber);
			rowNumber = rowNumber + 1;
	        if (tabName.equals("BATCH_DETAIL")) {
	                Object[] tableHeadingYear = {"Feed ID", "Batch ID", "Award Number", "Account Number", "Principal Investigator", "Type", "Updated By", "Updated On", "Feed Status", "Follow Up", "Comment"};
	                printService.prepareExcelSheetHeader(sheet, tableHeadingYear, workbook, tableBodyStyle, rowNumber++);
			} else if (tabName.equals("PENDING_FEEDS")) {
	                Object[] tableHeadingYear = {"Feed ID", "Award Number", "Account Number", "Principal Investigator", "Type", "Updated By", "Updated On", "Feed Status","Comment"};
	                printService.prepareExcelSheetHeader(sheet, tableHeadingYear, workbook, tableBodyStyle, rowNumber++);
			} else if (tabName.equals("BATCH_HISTORY")) {
	                Object[] tableHeadingYear = {"Batch ID", "No. of Awards", "No. of Error Awards", "Batch Generated on", "Response Processed on"};
	                printService.prepareExcelSheetHeader(sheet, tableHeadingYear, workbook, tableBodyStyle, rowNumber++);
			}
			prepareSapFeedReport(sapAwardFeeds, rowNumber, tabName, sheet, tableBodyStyle);
	        } catch (Exception e) {
				logger.error("error in prepareExcelSheetForSapFeedReport {}", e.getMessage());
			}
	}

	private Cell assignCell(int cellNumber, XSSFCellStyle tableBodyStyle, Row row) {
		Cell cell = row.createCell(cellNumber);
		cell.setCellStyle(tableBodyStyle);
		return cell;
	}

	private void prepareSapFeedReport(List<SapAwardFeed> sapAwardFeeds, int rowNumber, String tabName, XSSFSheet sheet,
			XSSFCellStyle tableBodyStyle) {
		if (tabName.equals("BATCH_DETAIL")) {
			prepareSapBatchDetail(rowNumber, sheet, tableBodyStyle, sapAwardFeeds);
        } else if (tabName.equals("PENDING_FEEDS")) {
        	prepareSapPendingFeedDetail(rowNumber, sheet, tableBodyStyle, sapAwardFeeds);
		} else if (tabName.equals("BATCH_HISTORY")) {
			prepareSapBatchHistoryDetail(rowNumber, sheet, tableBodyStyle, sapAwardFeeds);
		}
	}

	private void prepareSapBatchHistoryDetail(int rowNumber, XSSFSheet sheet, XSSFCellStyle tableBodyStyle,
			List<SapAwardFeed> sapAwardFeeds) {
		for(SapAwardFeed sapAwardFeed : sapAwardFeeds ) {
			int cellNumber = 0;
			Row row = sheet.createRow(rowNumber);
			Cell batchIdCell = assignCell(cellNumber, tableBodyStyle, row);
			batchIdCell.setCellValue(sapAwardFeed.getBatchId() != null ? sapAwardFeed.getBatchId().toString() : "");
			cellNumber++;
	
			Cell noOfAwardCell = assignCell(cellNumber, tableBodyStyle, row);
			noOfAwardCell.setCellValue(sapAwardFeed.getTotalAwards() != null ? sapAwardFeed.getTotalAwards().toString() : "");
			cellNumber++;
	
			Cell erroAwardNumberCell = assignCell(cellNumber, tableBodyStyle, row);
			erroAwardNumberCell.setCellValue(sapAwardFeed.getTotalErrorAwards() != null && !sapAwardFeed.getTotalErrorAwards().toString().equals("0") ? sapAwardFeed.getTotalErrorAwards().toString() : "No Error");
			cellNumber++;
	
			Cell createdOnCell = assignCell(cellNumber, tableBodyStyle, row);
			createdOnCell.setCellValue(sapAwardFeed.getCreateTimestamp() != null ? commonService.convertDateFormatBasedOnTimeZone(sapAwardFeed.getCreateTimestamp().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
			cellNumber++;
	
			Cell updatedOnCell = assignCell(cellNumber, tableBodyStyle, row);
			updatedOnCell.setCellValue(sapAwardFeed.getResponseTimestamp() != null ? commonService.convertDateFormatBasedOnTimeZone(sapAwardFeed.getResponseTimestamp().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
			cellNumber++;
			rowNumber++;
		}
	}

	private void prepareSapPendingFeedDetail(int rowNumber, XSSFSheet sheet, XSSFCellStyle tableBodyStyle,
			List<SapAwardFeed> sapAwardFeeds) {
		for(SapAwardFeed sapAwardFeed : sapAwardFeeds ) {
			int cellNumber = 0;
			Row row = sheet.createRow(rowNumber);
			Cell feedIdCell = assignCell(cellNumber, tableBodyStyle, row);
			feedIdCell.setCellValue(sapAwardFeed.getFeedId() != null ? sapAwardFeed.getFeedId().toString() : "");
			cellNumber++;
	
			Cell awardNumberCell = assignCell(cellNumber, tableBodyStyle, row);
			awardNumberCell.setCellValue(sapAwardFeed.getAwardNumber() != null ? sapAwardFeed.getAwardNumber() : "");
			cellNumber++;
	
			Cell accountNumberCell = assignCell(cellNumber, tableBodyStyle, row);
			accountNumberCell.setCellValue(sapAwardFeed.getAccountNumber() != null ? sapAwardFeed.getAccountNumber() : "");
			cellNumber++;
	
			Cell piCell = assignCell(cellNumber, tableBodyStyle, row);
			piCell.setCellValue(sapAwardFeed.getPiName() != null ? sapAwardFeed.getPiName() : "");
			cellNumber++;
	
			Cell typeCell = assignCell(cellNumber, tableBodyStyle, row);
			typeCell.setCellValue(sapAwardFeed.getFeedTypeDesc() != null ? sapAwardFeed.getFeedTypeDesc() :"");
			cellNumber++;
	
			Cell updatedByCell = assignCell(cellNumber, tableBodyStyle, row);
			updatedByCell.setCellValue(sapAwardFeed.getUpdateUserFullName() != null ? sapAwardFeed.getUpdateUserFullName() :"System");
			cellNumber++;
	
			Cell updatedOnCell = assignCell(cellNumber, tableBodyStyle, row);
			updatedOnCell.setCellValue(sapAwardFeed.getUpdateTimeStamp() != null ? commonService.convertDateFormatBasedOnTimeZone(sapAwardFeed.getUpdateTimeStamp().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
			cellNumber++;
	
			Cell statusCell = assignCell(cellNumber, tableBodyStyle, row);
			statusCell.setCellValue(sapAwardFeed.getFeedStatusDesc() != null ? sapAwardFeed.getFeedStatusDesc() : "");
			cellNumber++;
	
			Cell commentCell = assignCell(cellNumber, tableBodyStyle, row);
			commentCell.setCellValue(sapAwardFeed.getUserComment() != null ? sapAwardFeed.getUserComment() : "");
			cellNumber++;
			rowNumber++;
		}
	}

	private void prepareSapBatchDetail(int rowNumber, XSSFSheet sheet, XSSFCellStyle tableBodyStyle,
			List<SapAwardFeed> sapAwardFeeds) {
		for(SapAwardFeed sapAwardFeed : sapAwardFeeds ) {
			int cellNumber = 0;
			Row row = sheet.createRow(rowNumber);
			Cell feedIdCell = assignCell(cellNumber, tableBodyStyle, row);
			feedIdCell.setCellValue(sapAwardFeed.getFeedId() != null ? sapAwardFeed.getFeedId().toString() : "");
			cellNumber++;
	
			Cell batchIdCell = assignCell(cellNumber, tableBodyStyle, row);
			batchIdCell.setCellValue(sapAwardFeed.getBatchId() != null ? sapAwardFeed.getBatchId().toString() : "");
			cellNumber++;
	
			Cell awardNumberCell = assignCell(cellNumber, tableBodyStyle, row);
			awardNumberCell.setCellValue(sapAwardFeed.getAwardNumber() != null ? sapAwardFeed.getAwardNumber() : "");
			cellNumber++;
	
			Cell accountNumberCell = assignCell(cellNumber, tableBodyStyle, row);
			accountNumberCell.setCellValue(sapAwardFeed.getAccountNumber() != null ? sapAwardFeed.getAccountNumber() : "");
			cellNumber++;
	
			Cell piCell = assignCell(cellNumber, tableBodyStyle, row);
			piCell.setCellValue(sapAwardFeed.getPiName() != null ? sapAwardFeed.getPiName() : "");
			cellNumber++;
	
			Cell typeCell = assignCell(cellNumber, tableBodyStyle, row);
			typeCell.setCellValue(sapAwardFeed.getFeedTypeDesc() != null ? sapAwardFeed.getFeedTypeDesc() :"");
			cellNumber++;
	
			Cell updatedByCell = assignCell(cellNumber, tableBodyStyle, row);
			updatedByCell.setCellValue(sapAwardFeed.getUpdateUserFullName() != null ? sapAwardFeed.getUpdateUserFullName() :"System");
			cellNumber++;
	
			Cell updatedOnCell = assignCell(cellNumber, tableBodyStyle, row);
			updatedOnCell.setCellValue(sapAwardFeed.getUpdateTimeStamp() != null ? commonService.convertDateFormatBasedOnTimeZone(sapAwardFeed.getUpdateTimeStamp().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
			cellNumber++;
	
			Cell statusCell = assignCell(cellNumber, tableBodyStyle, row);
			statusCell.setCellValue(sapAwardFeed.getFeedStatusDesc() != null ? sapAwardFeed.getFeedStatusDesc() : "");
			cellNumber++;
		
			Cell followUpCell = assignCell(cellNumber, tableBodyStyle, row);
			followUpCell.setCellValue(sapAwardFeed.getUserActionDesc() != null ? sapAwardFeed.getUserActionDesc() : "");
			cellNumber++;
		
			Cell commentCell = assignCell(cellNumber, tableBodyStyle, row);
			commentCell.setCellValue(sapAwardFeed.getUserComment() != null ? sapAwardFeed.getUserComment() : "");
			cellNumber++;
			rowNumber++;
		}
	}

}
