package com.polus.fibicomp.currentandpending.service;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.TimeZone;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.polus.fibicomp.award.awardprojectoutcome.dao.AwardProjectOutcomeDao;
import com.polus.fibicomp.award.awardprojectoutcome.service.AwardProjectOutcomeService;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.currentandpending.dao.CurrentAndPendingDao;
import com.polus.fibicomp.currentandpending.dto.CurrentAndPendingModuleDTO;
import com.polus.fibicomp.currentandpending.dto.CurrentAndPendingPersonDTO;
import com.polus.fibicomp.currentandpending.pojo.CPReportHeader;
import com.polus.fibicomp.currentandpending.pojo.CPReportProjectDetail;
import com.polus.fibicomp.currentandpending.pojo.CPReportProjectDetailExt;
import com.polus.fibicomp.currentandpending.vo.CurrentAndPendingVO;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.lookup.dao.ProposalLookUpDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.rolodex.dao.RolodexDao;

@Transactional
@Service(value = "currentAndPendingService")
public class CurrentAndPendingServiceImpl implements CurrentAndPendingService {

	protected static Logger logger = LogManager.getLogger(CurrentAndPendingServiceImpl.class.getName());

	@Autowired
	private ProposalDao proposalDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private CurrentAndPendingDao currentAndPendingDao;

	@Autowired
	private AwardProjectOutcomeDao awardProjectOutcomeDao;

	@Autowired
	@Qualifier(value = "personDao")
	private PersonDao personDao;

	@Autowired
	private RolodexDao rolodexDao;

	@Autowired
	private ProposalLookUpDao proposalLookUpDao;

	@Autowired
	private AwardBudgetDao awardBudgetDao;

	@Autowired
	private BudgetDao budgetDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private AwardProjectOutcomeService awardProjectOutcomeService;

	@Autowired
	private ProposalModuleDao proposalModuleDao;

	@Override
	public String getPersonList(CurrentAndPendingVO currentAndPendingVO) {
		List<ProposalPerson> proposalPersons = proposalDao.fetchProposalPersonBasedOnProposalId(Integer.parseInt(currentAndPendingVO.getModuleItemKey()));
		String personId = null;
		if (proposalPersons != null && !proposalPersons.isEmpty()) {
			for (ProposalPerson proposalPerson : proposalPersons) {
				if (proposalPerson.getRolodexId() != null) {
					personId = proposalPerson.getRolodexId().toString();
				} else {
					personId = proposalPerson.getPersonId();
				}
				proposalPerson.setIsGenerated(currentAndPendingDao.checkIfReportGenerated(personId, currentAndPendingVO.getModuleItemKey()));
			}
			currentAndPendingVO.setProposalPersons(proposalPersons);
		}
		currentAndPendingVO.setProposalPersonRoles(proposalLookUpDao.fetchAllProposalPersonRoles());
		currentAndPendingVO.setCurrencyDetails(commonDao.fetchCurrencyDetails());
		currentAndPendingVO.setProposalFundingStatus(proposalLookUpDao.fetchAllProposalFundingStatus());
		currentAndPendingVO.setSponsorTypes(grantCallDao.fetchAllSponsorTypes());
		return commonDao.convertObjectToJSON(currentAndPendingVO);
	}

	@Override
	public String getCPDetailsForSelectedPersons(CurrentAndPendingVO currentAndPendingVO) {
		List<CurrentAndPendingPersonDTO> persons = currentAndPendingVO.getSelectedPersons();
		if (persons != null && !persons.isEmpty()) {
			for (CurrentAndPendingPersonDTO person : persons) {
				CPReportHeader oldCpReportHeader = currentAndPendingDao.getCPReportHeaderOfMaxVersionOfPerson(currentAndPendingVO.getModuleCode(), currentAndPendingVO.getModuleItemKey(), person.getPersonId());
				CPReportHeader newCpReportHeader = saveCPReportHeaderDetails(currentAndPendingVO, person);
				person.setCpReportHeader(newCpReportHeader);
				saveCPReportProjectDetails(person, newCpReportHeader, oldCpReportHeader, currentAndPendingVO.getUpdateUser());
			}
			currentAndPendingVO.setSelectedPersons(preparePersonCurrentAndPendingDetails(currentAndPendingVO));
		}
		return commonDao.convertObjectToJSON(currentAndPendingVO);
	}

	private CPReportHeader saveCPReportHeaderDetails(CurrentAndPendingVO currentAndPendingVO, CurrentAndPendingPersonDTO person) {
		CPReportHeader cpReportHeader = new CPReportHeader();
		cpReportHeader.setModuleCode(currentAndPendingVO.getModuleCode());
		cpReportHeader.setModuleItemId(currentAndPendingVO.getModuleItemKey());
		cpReportHeader.setVersionNumber(currentAndPendingDao.getMaxVersionNumberByParams(currentAndPendingVO.getModuleItemKey(), person.getPersonId()));
		cpReportHeader.setPersonId(person.getPersonId());
		cpReportHeader.setNonEmployeeFlag(person.getNonEmployeeFlag());
		cpReportHeader.setCreateTimestamp(commonDao.getCurrentTimestamp());
		cpReportHeader.setCreateUser(currentAndPendingVO.getCreateUser());
		cpReportHeader.setUpdateUser(currentAndPendingVO.getUpdateUser());
		cpReportHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		return currentAndPendingDao.saveOrUpdateCPReportHeader(cpReportHeader);
	}

	private void saveCPReportProjectDetails(CurrentAndPendingPersonDTO person, CPReportHeader newCpReportHeader, CPReportHeader oldCpReportHeader, String updateUser) {
		List<String> personIds = new ArrayList<>();
		List<Integer> rolodexIds = new ArrayList<>();
		List<CurrentAndPendingModuleDTO> currentAwards = new ArrayList<>();
		List<CurrentAndPendingModuleDTO> pendingProposals = new ArrayList<>();
		if (Boolean.TRUE.equals(person.getNonEmployeeFlag())) {
			rolodexIds.add(Integer.parseInt(person.getPersonId()));
		} else {
			personIds.add(person.getPersonId());
		}
		setPendingProposalDetails(person, pendingProposals, rolodexIds, personIds, newCpReportHeader, oldCpReportHeader, updateUser);
		setCurrentAwardDetails(person, currentAwards, rolodexIds, personIds, newCpReportHeader, oldCpReportHeader, updateUser);
		setExternalProjectDetails(newCpReportHeader, oldCpReportHeader, person.getIsGenerated(), updateUser);
		person.setLastUpdatedUserFullName(personDao.getUserFullNameByUserName(updateUser));
		if (newCpReportHeader != null) {
			person.setLastUpdatedTimestamp(newCpReportHeader.getUpdateTimestamp());
		} else {
			person.setLastUpdatedTimestamp(oldCpReportHeader.getUpdateTimestamp());
		}
	}

	private void setExternalProjectDetails(CPReportHeader newCpReportHeader, CPReportHeader oldCpReportHeader, Boolean isGenerated, String updateUser) {
		if (Boolean.TRUE.equals(isGenerated)) {
			List<CPReportProjectDetail> cpReportProjectDetails = currentAndPendingDao.getCPReportProjectDetailsByCPReportHeaderId(oldCpReportHeader.getCpReportHeaderId());
			for (CPReportProjectDetail projectDetail : cpReportProjectDetails) {
				CPReportProjectDetail cpProjectExternalDetail = new CPReportProjectDetail();
				if (Boolean.TRUE.equals(projectDetail.getIsManuallyAdded())) {
					saveCPProjectDetails(newCpReportHeader, cpProjectExternalDetail, projectDetail, updateUser);
				}
			}
		}
	}

	private void setPendingProposalDetails(CurrentAndPendingPersonDTO person, List<CurrentAndPendingModuleDTO> pendingProposals, List<Integer> rolodexIds,	List<String> personIds, CPReportHeader newCpReportHeader, CPReportHeader oldCpReportHeader, String updateUser) {
		List<Integer> proposalIds = awardProjectOutcomeDao.getProposalIdBasedOnParams(personIds, rolodexIds);
		BigDecimal totalCost = BigDecimal.ZERO;
		BigDecimal totalDirectCost = BigDecimal.ZERO;
		if (proposalIds != null && !proposalIds.isEmpty()) {
			List<Integer> proposalStatuses = new ArrayList<>();
			proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS);
			proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_UNSUCCESSFUL);
			proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_INACTIVE);
			proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_REVISION_REQUESTED);
			proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_HOD_RETURNED);
			proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_GRANT_ADMIN_RETURNED);
			proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_GRANT_MANAGER_RETURNED);
			proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_NOT_SUBMITTED);
			proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_RETURNED);
			proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_WITHDRAW);
			proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_AWARDED);
			List<Proposal> proposals = awardProjectOutcomeDao.getProposalsBasedOnParams(proposalIds, proposalStatuses, null);
			for (Proposal proposal : proposals) {
				List<String> moduleDetailIds = new ArrayList<>();
				String proposalId = proposal.getProposalId().toString();
				if (!proposalId.equals(person.getModuleItemId())) {
					CPReportProjectDetail cpReportProjectDetail = new CPReportProjectDetail();
					CurrentAndPendingModuleDTO pendingProposal = new CurrentAndPendingModuleDTO();
					preparePendingProposalDetail(pendingProposal, proposal, person, totalCost, totalDirectCost);
						if (Boolean.TRUE.equals(person.getIsGenerated())) {
						List<CPReportProjectDetail> cpReportProjectDetails = currentAndPendingDao.getCPReportProjectDetailsByCPReportHeaderId(oldCpReportHeader.getCpReportHeaderId());
						CPReportProjectDetail cpReportProject = new CPReportProjectDetail();
						cpReportProject.setLinkedModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
						cpReportProject.setLinkedModuleItemId(proposalId);
						for (CPReportProjectDetail projectDetail : cpReportProjectDetails) {
							if (projectDetail.getLinkedModuleItemId() != null && projectDetail.getLinkedModuleCode().equals(Constants.DEV_PROPOSAL_MODULE_CODE) && proposalId.equals(projectDetail.getLinkedModuleItemId())) {
								saveCPProjectDetails(newCpReportHeader, cpReportProject, projectDetail, updateUser);
								moduleDetailIds.add(projectDetail.getLinkedModuleItemId());
								saveProjectExtDetails(cpReportProject, pendingProposal, updateUser);
								setCPModuleDetail(pendingProposal, cpReportProject);
							}
						}
						if (!moduleDetailIds.isEmpty()) {
							for (String moduleDetailId : moduleDetailIds) {
								if (!proposalId.equals(moduleDetailId)) {
									saveCPReportProjectDetails(newCpReportHeader, cpReportProject, updateUser);
									saveProjectExtDetails(cpReportProject, pendingProposal, updateUser);
									setCPModuleDetail(pendingProposal, cpReportProject);
								}
							}
						} else {
							saveCPReportProjectDetails(newCpReportHeader, cpReportProject, updateUser);
							saveProjectExtDetails(cpReportProject, pendingProposal, updateUser);
							setCPModuleDetail(pendingProposal, cpReportProject);
						}
					} else {
						cpReportProjectDetail.setLinkedModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
						cpReportProjectDetail.setLinkedModuleItemId(proposalId);
						cpReportProjectDetail = saveCPReportProjectDetails(newCpReportHeader, cpReportProjectDetail, updateUser);
						saveProjectExtDetails(cpReportProjectDetail, pendingProposal, updateUser);
						setCPModuleDetail(pendingProposal, cpReportProjectDetail);
					}
					pendingProposals.add(pendingProposal);
				}
			}
			person.setPendingProposals(pendingProposals);
		}
	}

	private CPReportProjectDetail saveCPProjectDetails(CPReportHeader newCpReportHeader, CPReportProjectDetail cpReportProject, CPReportProjectDetail projectDetail, String updateUser) {
		cpReportProject.setCpReportHeader(newCpReportHeader);
		cpReportProject.setIsExcluded(projectDetail.getIsExcluded());
		cpReportProject.setIsManuallyAdded(projectDetail.getIsManuallyAdded());
		cpReportProject.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		cpReportProject.setUpdateUser(updateUser);
		cpReportProject.setLinkedModuleCode(projectDetail.getLinkedModuleCode());
		cpReportProject.setPercentageEffort(projectDetail.getPercentageEffort());
		cpReportProject.setGrantCallName(projectDetail.getGrantCallName());
		cpReportProject.setTitle(projectDetail.getTitle());
		cpReportProject.setTotalAwardAmount(projectDetail.getTotalAwardAmount());
		cpReportProject.setStartDate(projectDetail.getStartDate());
		cpReportProject.setEndDate(projectDetail.getEndDate());
		cpReportProject.setSponsorCode(projectDetail.getSponsorCode());
		cpReportProject.setCurrencyCode(projectDetail.getCurrencyCode());
		cpReportProject.setPersonRoleId(projectDetail.getPersonRoleId());
		cpReportProject.setFundingStatusCode(projectDetail.getFundingStatusCode());
		cpReportProject.setSponsorTypeCode(projectDetail.getSponsorTypeCode());
		return currentAndPendingDao.saveOrUpdateCPReportProjectDetail(cpReportProject);
	}

	private CurrentAndPendingModuleDTO preparePendingProposalDetail(CurrentAndPendingModuleDTO pendingProposal, Proposal proposal, CurrentAndPendingPersonDTO person, BigDecimal totalCost, BigDecimal totalDirectCost) {
		pendingProposal.setEndDate(proposal.getEndDate());
		pendingProposal.setStartDate(proposal.getStartDate());
		pendingProposal.setModuleItemId(proposal.getProposalId());
		pendingProposal.setDepartment(commonDao.getUnitName(proposal.getHomeUnitNumber()));
		pendingProposal.setTitle(proposal.getTitle());
		if (commonDao.getParameterValueAsBoolean(Constants.IS_APPLICATION_ID_REQUIRED)) {
			if (proposal.getApplicationId() != null) {
				pendingProposal.setApplicationId(proposal.getApplicationId());
			}
		}
		pendingProposal.setModuleStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(proposal.getStatusCode()).getDescription());
		BudgetHeader budgetHeader = budgetDao.getMaxBudgetVersionOfBudget(proposal.getProposalId());
		if (budgetHeader != null) {
			totalCost = budgetHeader.getTotalCost();
			totalDirectCost = budgetHeader.getTotalDirectCost();
		}
		pendingProposal.setTotalAwardAmount(totalCost);
		pendingProposal.setAnnualDirectCost(totalDirectCost);
		List<ProposalPerson> proposalPersons = proposalDao.fetchProposalPersonBasedOnProposalId(proposal.getProposalId());
		if (proposalPersons != null && !proposalPersons.isEmpty()) {
			for (ProposalPerson proposalPerson : proposalPersons) {
				if (proposalPerson.getRolodexId() != null && proposalPerson.getRolodexId().toString().equals(person.getPersonId())	|| proposalPerson.getPersonId() != null	&& proposalPerson.getPersonId().equals(person.getPersonId())) {
					pendingProposal.setPercentageEffort(proposalPerson.getPercentageOfEffort());
					pendingProposal.setPersonRoleId(proposalPerson.getPersonRoleId());
				}
				if (proposalPerson.getPersonRoleId().equals(Constants.PI_ROLE_CODE)) {
					if (proposalPerson.getPersonId() != null) {
						pendingProposal.setLeadPIPersonId(proposalPerson.getPersonId());
						pendingProposal.setLeadPiNonEmployeeFlag(false);
					} else {
						pendingProposal.setLeadPIPersonId(proposalPerson.getRolodexId().toString());
						pendingProposal.setLeadPiNonEmployeeFlag(true);
					}
					pendingProposal.setLeadPrincipalInvestigator(proposalPerson.getFullName());
				}
			}
		}
		pendingProposal.setSponsor(commonDao.getSponsorById(proposal.getSponsorCode()));
		return pendingProposal;
	}

	private void saveProjectExtDetails(CPReportProjectDetail cpReportProjectDetail, CurrentAndPendingModuleDTO moduleDetail, String updateUser) {
		CPReportProjectDetailExt cpReportProjectDetailExt = new CPReportProjectDetailExt();
		cpReportProjectDetailExt.setCpReportProjectDetailId(cpReportProjectDetail.getCpReportProjectDetailId());
		cpReportProjectDetailExt.setPercentageOfEffort(moduleDetail.getPercentageEffort());
		cpReportProjectDetailExt.setLeadPIPersonId(moduleDetail.getLeadPIPersonId());
		cpReportProjectDetailExt.setLeadPiNonEmployeeFlag(moduleDetail.getLeadPiNonEmployeeFlag());
		cpReportProjectDetailExt.setPersonRoleId(moduleDetail.getPersonRoleId());
		cpReportProjectDetailExt.setTotalAwardAmount(moduleDetail.getTotalAwardAmount());
		cpReportProjectDetailExt.setAnnualDirectCost(moduleDetail.getAnnualDirectCost());
		cpReportProjectDetailExt.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		cpReportProjectDetailExt.setUpdateUser(updateUser);
		currentAndPendingDao.saveOrUpdateCPReportProjectDetailExt(cpReportProjectDetailExt);
	}

	private void setCurrentAwardDetails(CurrentAndPendingPersonDTO person, List<CurrentAndPendingModuleDTO> currentAwards, List<Integer> rolodexIds, List<String> personIds, CPReportHeader newCpReportHeader, CPReportHeader oldCpReportHeader, String updateUser) {
		BigDecimal totalCost = BigDecimal.ZERO;
		BigDecimal totalDirectCost = BigDecimal.ZERO;
		List<Integer> awardIds = awardProjectOutcomeDao.getAwardIdBasedOnParams(personIds, rolodexIds);
		if (awardIds != null && !awardIds.isEmpty()) {
			List<Award> awards = awardProjectOutcomeDao.getAwardsBasedOnParams(awardIds, null, null, checkAwardEndYearWithinFiveYear());
			for (Award award : awards) {
					String awardId = award.getAwardId().toString();
					List<String> moduleDetailIds = new ArrayList<>();
					CPReportProjectDetail cpReportProjectDetail = new CPReportProjectDetail();
					CurrentAndPendingModuleDTO currentAward = new CurrentAndPendingModuleDTO();
					prepareCurrentAwardDetail(currentAward, award, person, totalCost, totalDirectCost);
						if (Boolean.TRUE.equals(person.getIsGenerated())) {
						List<CPReportProjectDetail> cpReportProjectDetails = currentAndPendingDao.getCPReportProjectDetailsByCPReportHeaderId(oldCpReportHeader.getCpReportHeaderId());
						CPReportProjectDetail cpReportProject = new CPReportProjectDetail();
						cpReportProject.setLinkedModuleCode(Constants.AWARD_MODULE_CODE);
						cpReportProject.setLinkedModuleItemId(awardId);
						for (CPReportProjectDetail projectDetail : cpReportProjectDetails) {
							if (projectDetail.getLinkedModuleItemId() != null && projectDetail.getLinkedModuleCode().equals(Constants.AWARD_MODULE_CODE)	&& awardId.equals(projectDetail.getLinkedModuleItemId())) {
								saveCPProjectDetails(newCpReportHeader, cpReportProject, projectDetail, updateUser);
								moduleDetailIds.add(projectDetail.getLinkedModuleItemId());
								saveProjectExtDetails(cpReportProject, currentAward, updateUser);
								setCPModuleDetail(currentAward, cpReportProject);
							}
						}
						if (!moduleDetailIds.isEmpty()) {
							for (String moduleDetailId : moduleDetailIds) {
								if (!awardId.equals(moduleDetailId)) {
									saveCPReportProjectDetails(newCpReportHeader, cpReportProject, updateUser);
									saveProjectExtDetails(cpReportProject, currentAward, updateUser);
									setCPModuleDetail(currentAward, cpReportProject);
								}
							}
						} else {
							saveCPReportProjectDetails(newCpReportHeader, cpReportProject, updateUser);
							saveProjectExtDetails(cpReportProject, currentAward, updateUser);
							setCPModuleDetail(currentAward, cpReportProject);
						}
					} else {
						cpReportProjectDetail.setLinkedModuleCode(Constants.AWARD_MODULE_CODE);
						cpReportProjectDetail.setLinkedModuleItemId(awardId);
						cpReportProjectDetail = saveCPReportProjectDetails(newCpReportHeader, cpReportProjectDetail, updateUser);
						saveProjectExtDetails(cpReportProjectDetail, currentAward, updateUser);
						setCPModuleDetail(currentAward, cpReportProjectDetail);
					}
					currentAwards.add(currentAward);
			}
			person.setCurrentAwards(currentAwards);
		}
	}

	private CurrentAndPendingModuleDTO prepareCurrentAwardDetail(CurrentAndPendingModuleDTO currentAward, Award award, CurrentAndPendingPersonDTO person, BigDecimal totalCost, BigDecimal totalDirectCost) {
		currentAward.setModuleItemId(award.getAwardId());
		currentAward.setEndDate(award.getFinalExpirationDate());
		currentAward.setStartDate(award.getBeginDate());
		currentAward.setModuleItemKey(award.getAwardNumber());
		currentAward.setSponsorAwardNumber(award.getSponsorAwardNumber());
		currentAward.setDepartment(commonDao.getUnitName(award.getLeadUnitNumber()));
		currentAward.setTitle(award.getTitle());
		currentAward.setModuleStatus(awardDao.fetchAwardStatusByCode(award.getStatusCode()).getDescription());
		if (award.getSponsorCode() != null) {
			currentAward.setSponsor(commonDao.getSponsorById(award.getSponsorCode()));
		}
		totalCost = awardProjectOutcomeService.totalAwardGrantedAmount(award.getAwardId());
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getAwardBudgetHeaderByAwardId(award.getAwardId());
		if (awardBudgetHeader != null) {
			totalDirectCost = awardBudgetHeader.getTotalDirectCost();
		}
		currentAward.setTotalAwardAmount(totalCost);
		currentAward.setAnnualDirectCost(totalDirectCost);
		List<AwardPerson> awardPersons = awardDao.getAwardPersons(award.getAwardId());
		if (awardPersons != null && !awardPersons.isEmpty()) {
			for (AwardPerson awardPerson : awardPersons) {
				if ((awardPerson.getPersonId() != null && awardPerson.getPersonId().equals(person.getPersonId())) || (awardPerson.getRolodexId() != null && awardPerson.getRolodexId().toString().equals(person.getPersonId()))) {
					currentAward.setPercentageEffort(awardPerson.getPercentageEffort());
					currentAward.setPersonRoleId(awardPerson.getPersonRoleId());
				}
				if (awardPerson.getPersonRoleId() != null && awardPerson.getPersonRoleId().equals(Constants.PI_ROLE_CODE)) {
					if (awardPerson.getPersonId() != null) {
						currentAward.setLeadPIPersonId(awardPerson.getPersonId());
						currentAward.setLeadPiNonEmployeeFlag(false);
					} else {
						currentAward.setLeadPIPersonId(awardPerson.getRolodexId().toString());
						currentAward.setLeadPiNonEmployeeFlag(true);
					}
					currentAward.setLeadPrincipalInvestigator(awardPerson.getFullName());
				}
			}
		}
		return currentAward;
	}

	private CurrentAndPendingModuleDTO setCPModuleDetail(CurrentAndPendingModuleDTO moduleDetail, CPReportProjectDetail cpReportProjectDetail) {
		moduleDetail.setIsExcluded(cpReportProjectDetail.getIsExcluded());
		moduleDetail.setCpReportHeaderId(cpReportProjectDetail.getCpReportHeader().getCpReportHeaderId());
		moduleDetail.setCpReportProjectDetailId(cpReportProjectDetail.getCpReportProjectDetailId());
		CPReportProjectDetailExt cpReportProjectDetailExt = currentAndPendingDao.getCPReportProjectDetailExtById(cpReportProjectDetail.getCpReportProjectDetailId());
		if (cpReportProjectDetailExt != null) {
			cpReportProjectDetailExt.setProposalPersonRole(proposalDao.fetchProposalPersonRoles(cpReportProjectDetailExt.getPersonRoleId()));
			if (cpReportProjectDetailExt.getLeadPIPersonId() != null) {
				if (Boolean.TRUE.equals(cpReportProjectDetailExt.getLeadPiNonEmployeeFlag())) {
					cpReportProjectDetailExt.setLeadPrincipalInvestigator(rolodexDao.getRolodexDetailById(Integer.parseInt(cpReportProjectDetailExt.getLeadPIPersonId())).getFullName());
				} else {
					cpReportProjectDetailExt.setLeadPrincipalInvestigator(personDao.getPersonFullNameByPersonId(cpReportProjectDetailExt.getLeadPIPersonId()));
				}
			}
			moduleDetail.setCpReportProjectDetailExt(cpReportProjectDetailExt);
		}
		return moduleDetail;
	}

	private CPReportProjectDetail saveCPReportProjectDetails(CPReportHeader newCpReportHeader, CPReportProjectDetail cpReportProjectDetail, String updateUser) {
		cpReportProjectDetail.setIsExcluded(false);
		cpReportProjectDetail.setCpReportHeader(newCpReportHeader);
		cpReportProjectDetail.setIsManuallyAdded(false);
		cpReportProjectDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		cpReportProjectDetail.setUpdateUser(updateUser);
		return currentAndPendingDao.saveOrUpdateCPReportProjectDetail(cpReportProjectDetail);
	}

	@Override
	public String saveOrUpdateCPReportExtDetail(CurrentAndPendingVO currentAndPendingVO) {
		CPReportProjectDetailExt cpReportProjectDetailExt = currentAndPendingVO.getCpReportProjectDetailExt();
		cpReportProjectDetailExt.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		cpReportProjectDetailExt.setProposalPersonRole(proposalDao.fetchProposalPersonRoles(cpReportProjectDetailExt.getPersonRoleId()));
		if (Boolean.TRUE.equals(cpReportProjectDetailExt.getLeadPiNonEmployeeFlag())) {
			cpReportProjectDetailExt.setLeadPrincipalInvestigator(rolodexDao.getRolodexDetailById(Integer.parseInt(cpReportProjectDetailExt.getLeadPIPersonId())).getFullName());
		} else {
			cpReportProjectDetailExt.setLeadPrincipalInvestigator(personDao.getPersonFullNameByPersonId(cpReportProjectDetailExt.getLeadPIPersonId()));
		}
		currentAndPendingVO.setCpReportProjectDetailExt(currentAndPendingDao.saveOrUpdateCPReportProjectDetailExt(cpReportProjectDetailExt));
		return commonDao.convertObjectToJSON(currentAndPendingVO);
	}

	@Override
	public String excludeCurrentAndPendingDetails(CurrentAndPendingVO currentAndPendingVO) {
		CPReportProjectDetail cpReportProjectDetail = currentAndPendingDao.getCPReportProjectDetailsById(currentAndPendingVO.getCpReportProjectDetailId());
		cpReportProjectDetail.setIsExcluded(currentAndPendingVO.getIsExcluded());
		cpReportProjectDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		cpReportProjectDetail.setUpdateUser(currentAndPendingVO.getUpdateUser());
		currentAndPendingDao.saveOrUpdateCPReportProjectDetail(cpReportProjectDetail);
		return commonDao.convertObjectToJSON(currentAndPendingVO);
	}

	@Override
	public String getCurrentAndPendingDetails(CurrentAndPendingVO currentAndPendingVO) {
		currentAndPendingVO.setSelectedPersons(preparePersonCurrentAndPendingDetails(currentAndPendingVO));
		return commonDao.convertObjectToJSON(currentAndPendingVO);
	}

	@Override
	public List<CurrentAndPendingPersonDTO> preparePersonCurrentAndPendingDetails(CurrentAndPendingVO currentAndPendingVO) {
		List<CurrentAndPendingPersonDTO> selectedPersons = new ArrayList<>();
		List<String> personIds = new ArrayList<>();
		if (currentAndPendingVO.getPersonId() != null && !currentAndPendingVO.getPersonId().equals("")) {
			personIds.add(currentAndPendingVO.getPersonId());
		} else {
			personIds = currentAndPendingDao.fetchPersonIdsByParams(currentAndPendingVO.getModuleCode(), currentAndPendingVO.getModuleItemKey());
		}
		List<ProposalPerson> proposalPersons = proposalDao.fetchProposalPersonBasedOnProposalId(Integer.parseInt(currentAndPendingVO.getModuleItemKey()));
		for (String personId : personIds) {
			List<CurrentAndPendingModuleDTO> currentAwards = new ArrayList<>();
			List<CurrentAndPendingModuleDTO> pendingProposals = new ArrayList<>();
			CurrentAndPendingPersonDTO person = new CurrentAndPendingPersonDTO();
			CPReportHeader cpReportHeader = currentAndPendingDao.getCPReportHeaderOfMaxVersionOfPerson(currentAndPendingVO.getModuleCode(), currentAndPendingVO.getModuleItemKey(), personId);
			if (proposalPersons != null && !proposalPersons.isEmpty()) {
				for (ProposalPerson proposalPerson : proposalPersons) {
					if (proposalPerson.getRolodexId() != null && proposalPerson.getRolodexId().toString().equals(personId)	|| proposalPerson.getPersonId() != null	&& proposalPerson.getPersonId().equals(personId)) {
						person.setRoleName(proposalDao.fetchProposalPersonRoles(proposalPerson.getPersonRoleId()).getDescription());
					}
				}
			}
			person.setIsGenerated(currentAndPendingDao.checkIfReportGenerated(personId, currentAndPendingVO.getModuleItemKey()));
			person.setModuleItemId(currentAndPendingVO.getModuleItemKey());
			person.setPersonId(personId);
			person.setCpReportHeader(cpReportHeader);
			person.setCreateUser(cpReportHeader.getCreateUser());
			person.setNonEmployeeFlag(cpReportHeader.getNonEmployeeFlag());
			if (Boolean.TRUE.equals(cpReportHeader.getNonEmployeeFlag())) {
				person.setPersonName(rolodexDao.getRolodexDetailById(Integer.parseInt(personId)).getFullName());
			} else {
				person.setPersonName(personDao.getPersonFullNameByPersonId(personId));
			}
			List<CPReportProjectDetail> cpReportProjectDetails = currentAndPendingDao.getCPReportProjectDetailsByCPReportHeaderId(cpReportHeader.getCpReportHeaderId());
			List<Integer> proposalIds = new ArrayList<>();
			List<Integer> awardIds = new ArrayList<>();
			for (CPReportProjectDetail cpReportProjectDetail : cpReportProjectDetails) {
				if (cpReportProjectDetail.getLinkedModuleItemId() != null && cpReportProjectDetail.getLinkedModuleCode().equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
					proposalIds.add(Integer.parseInt(cpReportProjectDetail.getLinkedModuleItemId()));
				}
				if (cpReportProjectDetail.getLinkedModuleItemId() != null && cpReportProjectDetail.getLinkedModuleCode().equals(Constants.AWARD_MODULE_CODE)) {
					awardIds.add(Integer.parseInt(cpReportProjectDetail.getLinkedModuleItemId()));
				}
			}
			BigDecimal totalCost = BigDecimal.ZERO;
			BigDecimal totalDirectCost = BigDecimal.ZERO;
			if (awardIds != null && !awardIds.isEmpty()) {
				List<Award> awards = awardProjectOutcomeDao.getAwardsBasedOnParams(awardIds, null, null, checkAwardEndYearWithinFiveYear());
				if (awards != null && !awards.isEmpty()) {
					for (Award award : awards) {
						CurrentAndPendingModuleDTO currentAward = new CurrentAndPendingModuleDTO();
						prepareCurrentAwardDetail(currentAward, award, person, totalCost, totalDirectCost);
						List<CPReportProjectDetail> cpReportProjectDetailForAward = currentAndPendingDao.getCPReportProjectDetailsByCPReportHeaderId(cpReportHeader.getCpReportHeaderId());
						for (CPReportProjectDetail projectDetail : cpReportProjectDetailForAward) {
							if (projectDetail.getLinkedModuleItemId() != null && projectDetail.getLinkedModuleCode() != null && projectDetail.getLinkedModuleCode().equals(Constants.AWARD_MODULE_CODE)	&& award.getAwardId().toString().equals(projectDetail.getLinkedModuleItemId())) {
								setCPModuleDetail(currentAward, projectDetail);
							}
						}
						currentAwards.add(currentAward);
					}
				}
			}
			if (proposalIds != null && !proposalIds.isEmpty()) {
				List<Proposal> proposals = awardProjectOutcomeDao.getProposalsBasedOnParams(proposalIds, null, null);
				if (proposals != null && !proposals.isEmpty()) {
					for (Proposal proposal : proposals) {
						CurrentAndPendingModuleDTO pendingProposal = new CurrentAndPendingModuleDTO();
						preparePendingProposalDetail(pendingProposal, proposal, person, totalCost, totalDirectCost);
						List<CPReportProjectDetail> cpReportProjectDetailForProposal = currentAndPendingDao.getCPReportProjectDetailsByCPReportHeaderId(cpReportHeader.getCpReportHeaderId());
						for (CPReportProjectDetail projectDetail : cpReportProjectDetailForProposal) {
							if (projectDetail.getLinkedModuleItemId() != null && projectDetail.getLinkedModuleCode() != null && projectDetail.getLinkedModuleCode().equals(Constants.DEV_PROPOSAL_MODULE_CODE) && proposal.getProposalId().toString().equals(projectDetail.getLinkedModuleItemId())) {
								setCPModuleDetail(pendingProposal, projectDetail);
							}
						}
						pendingProposals.add(pendingProposal);
					}
				}
			}
			addExternalProjectDetail(cpReportHeader.getCpReportHeaderId(), currentAwards, pendingProposals);
			person.setCurrentAwards(currentAwards);
			person.setPendingProposals(pendingProposals);
			person.setLastUpdatedUserFullName(personDao.getUserFullNameByUserName(cpReportHeader.getUpdateUser()));
			person.setLastUpdatedTimestamp(cpReportHeader.getUpdateTimestamp());
			selectedPersons.add(person);
			Collections.sort(selectedPersons, (selectedPersonsOne, selectedPersonsTwo) -> selectedPersonsTwo.getLastUpdatedTimestamp().compareTo(selectedPersonsOne.getLastUpdatedTimestamp()));
		}
		return selectedPersons;
	}

	private void addExternalProjectDetail(Integer cpReportHeaderId, List<CurrentAndPendingModuleDTO> currentAwards, List<CurrentAndPendingModuleDTO> pendingProposals) {
		List<CPReportProjectDetail> cpReportProjectDetails = currentAndPendingDao.getCPReportProjectDetailsByCPReportHeaderId(cpReportHeaderId);
		for (CPReportProjectDetail cpReportProjectDetail : cpReportProjectDetails) {
			if (Boolean.TRUE.equals(cpReportProjectDetail.getIsManuallyAdded())) {
				if (cpReportProjectDetail.getLinkedModuleCode().equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
					pendingProposals.add(setExternalModuleDetail(cpReportProjectDetail));
				} else {
					currentAwards.add(setExternalModuleDetail(cpReportProjectDetail));
				}
			}
		}
	}

	public CurrentAndPendingModuleDTO setExternalModuleDetail(CPReportProjectDetail cpReportProjectDetail) {
		CurrentAndPendingModuleDTO moduleDetail = new CurrentAndPendingModuleDTO();
		moduleDetail.setTitle(cpReportProjectDetail.getTitle());
		moduleDetail.setStartDate(cpReportProjectDetail.getStartDate());
		moduleDetail.setEndDate(cpReportProjectDetail.getEndDate());
		moduleDetail.setIsExcluded(cpReportProjectDetail.getIsExcluded());
		moduleDetail.setIsManuallyAdded(cpReportProjectDetail.getIsManuallyAdded());
		moduleDetail.setTotalAwardAmount(cpReportProjectDetail.getTotalAwardAmount());
		moduleDetail.setPercentageEffort(cpReportProjectDetail.getPercentageEffort());
		moduleDetail.setGrantCallName(cpReportProjectDetail.getGrantCallName());
		moduleDetail.setCpReportProjectDetailId(cpReportProjectDetail.getCpReportProjectDetailId());
		moduleDetail.setLinkedModuleCode(cpReportProjectDetail.getLinkedModuleCode());
		if (cpReportProjectDetail.getSponsorCode() != null) {
			moduleDetail.setSponsorCode(cpReportProjectDetail.getSponsorCode());
			moduleDetail.setSponsor(commonDao.getSponsorById(cpReportProjectDetail.getSponsorCode()));
		} else if (cpReportProjectDetail.getSponsorName() != null) {
			Sponsor sponsor = new Sponsor();
			sponsor.setSponsorName(cpReportProjectDetail.getSponsorName());
			moduleDetail.setSponsor(sponsor);
		}
		if (cpReportProjectDetail.getCurrencyCode() != null) {
			moduleDetail.setCurrencyCode(cpReportProjectDetail.getCurrencyCode());
			moduleDetail.setCurrency(commonDao.getCurrencyByCurrencyCode(cpReportProjectDetail.getCurrencyCode()));
		}
		if (cpReportProjectDetail.getPersonRoleId() != null) {
			moduleDetail.setPersonRoleId(cpReportProjectDetail.getPersonRoleId());
			moduleDetail.setProposalPersonRole(proposalDao.fetchProposalPersonRoles(cpReportProjectDetail.getPersonRoleId()));
		}
		if (cpReportProjectDetail.getFundingStatusCode() != null) {
			moduleDetail.setFundingStatusCode(cpReportProjectDetail.getFundingStatusCode());
			moduleDetail.setModuleStatus(proposalModuleDao.fetchProposalFundingStatusById(cpReportProjectDetail.getFundingStatusCode()).getDescription());
		}
		if (cpReportProjectDetail.getSponsorTypeCode() != null) {
			moduleDetail.setSponsorTypeCode(cpReportProjectDetail.getSponsorTypeCode());
			moduleDetail.setSponsorType(proposalModuleDao.fetchSponsorTypeById(cpReportProjectDetail.getSponsorTypeCode()));
		}
		return moduleDetail;
	}

	public Timestamp checkAwardEndYearWithinFiveYear() {
		Calendar cal = Calendar.getInstance();
        cal.setTimeZone(TimeZone.getTimeZone("UTC"));
        cal.add(Calendar.YEAR, -5);
        return new Timestamp(cal.getTimeInMillis());
        
	}

	@Override
	public String saveOrUpdateCPExternalProjectDetail(CurrentAndPendingVO currentAndPendingVO) {
		CPReportProjectDetail cpReportProjectDetail = currentAndPendingVO.getExternalProjectDetail();
		cpReportProjectDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		currentAndPendingDao.saveOrUpdateCPReportProjectDetail(cpReportProjectDetail);
		if (cpReportProjectDetail.getSponsorCode() != null) {
			cpReportProjectDetail.setSponsor(commonDao.getSponsorById(cpReportProjectDetail.getSponsorCode()));
		} else {
			Sponsor sponsor = new Sponsor();
			sponsor.setSponsorName(cpReportProjectDetail.getSponsorName());
			cpReportProjectDetail.setSponsor(sponsor);
		}
		if (cpReportProjectDetail.getCurrencyCode() != null) {
			cpReportProjectDetail.setCurrency(commonDao.getCurrencyByCurrencyCode(cpReportProjectDetail.getCurrencyCode()));
		}
		if (cpReportProjectDetail.getPersonRoleId() != null) {
			cpReportProjectDetail.setProposalPersonRole(proposalDao.fetchProposalPersonRoles(cpReportProjectDetail.getPersonRoleId()));
		}
		if (cpReportProjectDetail.getFundingStatusCode() != null) {
			cpReportProjectDetail.setModuleStatus(proposalModuleDao.fetchProposalFundingStatusById(cpReportProjectDetail.getFundingStatusCode()).getDescription());
		}
		if (cpReportProjectDetail.getSponsorTypeCode() != null) {
			cpReportProjectDetail.setSponsorType(proposalModuleDao.fetchSponsorTypeById(cpReportProjectDetail.getSponsorTypeCode()));
		}
		currentAndPendingVO.setExternalProjectDetail(currentAndPendingDao.saveOrUpdateCPReportProjectDetail(cpReportProjectDetail));
		return commonDao.convertObjectToJSON(currentAndPendingVO);
	}

	@Override
	public String deleteCPExternalProjectDetail(CurrentAndPendingVO currentAndPendingVO) {
		currentAndPendingVO.setMessage(currentAndPendingDao.deleteCPExternalProjectDetail(currentAndPendingVO.getCpReportProjectDetailId()));
		return commonDao.convertObjectToJSON(currentAndPendingVO);
	}

}
