package com.polus.fibicomp.mobile.dao;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.view.MobileProposalView;
import com.polus.fibicomp.vo.CommonVO;

@SuppressWarnings("deprecation")
@Transactional
@Service(value = "fibiMobileDao")
public class FibiMobileDaoImpl implements FibiMobileDao {

	protected static Logger logger =  LogManager.getLogger(FibiMobileDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List<Object[]> getFibiSummaryTable(String personId, String unitNumber, boolean isAdmin, List<Object[]> summaryTable) {
		List<Object[]> subPropCount = null;
		List<Object[]> inPropCount = null;
		Query submittedProposal = null;
		Query inprogressProposal = null;

		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		if (isAdmin) {
			if (unitNumber != null) {
				submittedProposal = session.createSQLQuery(
						"SELECT 'Submitted Proposals' AS SUBMITTED_PROPOSAL, COUNT(T1.PROPOSAL_ID) AS COUNT, SUM(T2.TOTAL_COST) AS TOTAL_AMOUNT FROM EPS_PROPOSAL T1 LEFT OUTER JOIN BUDGET_HEADER T2 ON T1.BUDGET_HEADER_ID=T2.BUDGET_HEADER_ID WHERE T1.STATUS_CODE=2 AND (T1.HOME_UNIT_NUMBER IN( SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME ='VIEW_PROPOSAL' AND PERSON_ID = :person_id AND UNIT_NUMBER = :unitNumber) OR T1.PROPOSAL_ID IN (SELECT T1.PROPOSAL_ID FROM EPS_PROPOSAL_PERSONS T1 INNER JOIN EPS_PROP_PERSON_UNITS T2 ON T1.PROPOSAL_PERSON_ID = T2.PROPOSAL_PERSON_ID WHERE T1.PERSON_ID = :person_id AND T1.PROP_PERSON_ROLE_ID IN (1,2,3) AND T2.UNIT_NUMBER = :unitNumber))");				submittedProposal.setString("unitNumber", unitNumber);
			} else {
				submittedProposal = session.createSQLQuery(
						"SELECT 'Submitted Proposals' AS SUBMITTED_PROPOSAL, COUNT(T1.PROPOSAL_ID) AS COUNT, SUM(T2.TOTAL_COST) AS TOTAL_AMOUNT FROM EPS_PROPOSAL T1 LEFT OUTER JOIN BUDGET_HEADER T2 ON T1.BUDGET_HEADER_ID=T2.BUDGET_HEADER_ID WHERE T1.STATUS_CODE=2 AND (T1.HOME_UNIT_NUMBER IN( SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME ='VIEW_PROPOSAL' AND PERSON_ID = :person_id) OR T1.PROPOSAL_ID IN (SELECT T1.PROPOSAL_ID FROM EPS_PROPOSAL_PERSONS T1 WHERE T1.PERSON_ID = :person_id AND T1.PROP_PERSON_ROLE_ID IN (1,2,3)))");			}
		} else {
			submittedProposal = session.createSQLQuery(
					"SELECT 'Submitted Proposals' AS SUBMITTED_PROPOSAL, COUNT(T1.PROPOSAL_ID) AS COUNT, SUM(T2.TOTAL_COST) AS TOTAL_AMOUNT FROM EPS_PROPOSAL T1 LEFT OUTER JOIN BUDGET_HEADER T2 ON T1.BUDGET_HEADER_ID=T2.BUDGET_HEADER_ID WHERE T1.STATUS_CODE=2 AND T1.HOME_UNIT_NUMBER IN( SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME ='VIEW_PROPOSAL' AND PERSON_ID = :person_id)");		}
		submittedProposal.setString("person_id", personId);
		subPropCount = submittedProposal.list();
		if (subPropCount != null && !subPropCount.isEmpty()) {
			summaryTable.addAll(subPropCount);
		}
		if (isAdmin) {
			if (unitNumber != null) {
				inprogressProposal = session.createSQLQuery(
						"SELECT 'In Progress Proposals' AS IN_PROGRESS_PROPOSAL, COUNT(T1.PROPOSAL_ID) AS COUNT, SUM(T2.TOTAL_COST) AS TOTAL_AMOUNT FROM EPS_PROPOSAL T1 LEFT OUTER JOIN BUDGET_HEADER T2 ON T1.BUDGET_HEADER_ID=T2.BUDGET_HEADER_ID WHERE T1.STATUS_CODE = 1 AND (T1.HOME_UNIT_NUMBER IN( SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME ='VIEW_PROPOSAL' AND PERSON_ID = :person_id AND UNIT_NUMBER = :unitNumber) OR T1.PROPOSAL_ID IN ( SELECT T1.PROPOSAL_ID FROM EPS_PROPOSAL_PERSONS T1 INNER JOIN EPS_PROP_PERSON_UNITS T2 ON T1.PROPOSAL_PERSON_ID = T2.PROPOSAL_PERSON_ID WHERE T1.PERSON_ID = :person_id AND T1.PROP_PERSON_ROLE_ID IN (1,2,3) AND T2.UNIT_NUMBER = :unitNumber))");				inprogressProposal.setString("unitNumber", unitNumber);
			} else {
				inprogressProposal = session.createSQLQuery(
						"SELECT 'In Progress Proposals' AS IN_PROGRESS_PROPOSAL, COUNT(T1.PROPOSAL_ID) AS COUNT, SUM(T2.TOTAL_COST) AS TOTAL_AMOUNT FROM EPS_PROPOSAL T1 LEFT OUTER JOIN BUDGET_HEADER T2 ON T1.BUDGET_HEADER_ID=T2.BUDGET_HEADER_ID WHERE T1.STATUS_CODE = 1 AND (T1.HOME_UNIT_NUMBER IN( SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME ='VIEW_PROPOSAL' AND PERSON_ID = :person_id) OR T1.PROPOSAL_ID IN ( SELECT T1.PROPOSAL_ID FROM EPS_PROPOSAL_PERSONS T1 WHERE T1.PERSON_ID = :person_id AND T1.PROP_PERSON_ROLE_ID IN (1,2,3)))");			}
		} else {
			inprogressProposal = session.createSQLQuery(
					"SELECT 'In Progress Proposals' AS IN_PROGRESS_PROPOSAL, COUNT(T1.PROPOSAL_ID) AS COUNT, SUM(T2.TOTAL_COST) AS TOTAL_AMOUNT FROM EPS_PROPOSAL T1 LEFT OUTER JOIN BUDGET_HEADER T2 ON T1.BUDGET_HEADER_ID=T2.BUDGET_HEADER_ID WHERE T1.STATUS_CODE=1 AND T1.HOME_UNIT_NUMBER IN( SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME ='VIEW_PROPOSAL' AND PERSON_ID = :person_id)");		}
		inprogressProposal.setString("person_id", personId);
		inPropCount = inprogressProposal.list();
		if (inPropCount != null && !inPropCount.isEmpty()) {
			summaryTable.addAll(inPropCount);
		}
		return summaryTable;
	}

	@SuppressWarnings("unused")
	@Override
	public List<MobileProposalView> getProposalsByParams(CommonVO vo) {
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		String property3 = vo.getProperty3();
		String property4 = vo.getProperty4();
		String personId = vo.getPersonId();
		List<MobileProposalView> proposalViews = null;

		/*
		 * Conjunction and = Restrictions.conjunction(); try {
		 * logger.info("---------- getProposalsByParams ------------"); Session session
		 * = hibernateTemplate.getSessionFactory().getCurrentSession(); Criteria
		 * searchCriteria = session.createCriteria(ProposalView.class);
		 * searchCriteria.addOrder(Order.desc("updateTimeStamp")); if (property1 != null
		 * && !property1.isEmpty()) { and.add(Restrictions.like("proposalNumber", "%" +
		 * property1 + "%").ignoreCase()); } if (property2 != null &&
		 * !property2.isEmpty()) { and.add(Restrictions.like("title", "%" + property2 +
		 * "%").ignoreCase()); } if (property3 != null && !property3.isEmpty()) {
		 * and.add(Restrictions.like("leadUnit", "%" + property3 + "%").ignoreCase()); }
		 * if (property4 != null && !property4.isEmpty()) {
		 * and.add(Restrictions.like("sponsor", "%" + property4 + "%").ignoreCase()); }
		 * if (personId != null && !personId.isEmpty()) {
		 * searchCriteria.add(Restrictions.eq("personId", personId)); }
		 * searchCriteria.add(and);
		 * 
		 * @SuppressWarnings("unchecked") List<ProposalView> proposals =
		 * searchCriteria.list(); if (proposals != null && !proposals.isEmpty()) {
		 * proposalViews = new ArrayList<MobileProposalView>(); for (ProposalView
		 * proposal : proposals) { if (proposal.getProposalPersonRoleCode() == null ||
		 * proposal.getProposalPersonRoleCode().equals("PI")) { MobileProposalView
		 * mobileProposal = new MobileProposalView();
		 * mobileProposal.setDocumentNo(proposal.getDocumentNumber());
		 * mobileProposal.setLeadUnit(proposal.getLeadUnit());
		 * mobileProposal.setLeadUnitNo(proposal.getLeadUnitNumber());
		 * mobileProposal.setPi(proposal.getFullName());
		 * mobileProposal.setProposalNo(proposal.getProposalNumber());
		 * mobileProposal.setSponsor(proposal.getSponsor());
		 * mobileProposal.setStatus(proposal.getStatus());
		 * mobileProposal.setTitle(proposal.getTitle());
		 * mobileProposal.setVersionNo(String.valueOf(proposal.getVersionNumber()));
		 * mobileProposal.setCertified(proposal.isCertified());
		 * mobileProposal.setProposalPersonRoleId(proposal.getProposalPersonRoleCode());
		 * if (proposal.getStatusCode() == 1 && proposal.getProposalPersonRoleCode() !=
		 * null) { String hierarchyName =
		 * getSponsorHierarchy(proposal.getSponsorCode()); Criteria roleCriteria =
		 * session.createCriteria(ProposalPersonRole.class);
		 * roleCriteria.add(Restrictions.eq("code",
		 * proposal.getProposalPersonRoleCode()));
		 * roleCriteria.add(Restrictions.eq("sponsorHierarchyName", hierarchyName));
		 * ProposalPersonRole personRole = (ProposalPersonRole)
		 * roleCriteria.uniqueResult(); if (personRole != null) {
		 * mobileProposal.setCertificationRequired(personRole.getCertificationRequired()
		 * ); mobileProposal.setRoleName(personRole.getDescription()); }
		 * mobileProposal.setActionRequestCode("C"); } if (proposal.getStatusCode() ==
		 * 2) { mobileProposal.setActionRequestCode("A"); }
		 * mobileProposal.setPersonId(proposal.getPersonId());
		 * mobileProposal.setPersonName( hibernateTemplate.get(PrincipalBo.class,
		 * proposal.getPersonId()).getPrincipalName());
		 * mobileProposal.setProposalPersonRoleId(proposal.getProposalPersonRoleCode());
		 * proposalViews.add(mobileProposal); } } } } catch (Exception e) {
		 * logger.error("Error in method getProposalsByParams", e); e.printStackTrace();
		 * }
		 */
		return proposalViews;
	}

	@Override
	public List<MobileProposalView> getProposalsForCertification(String personId) {
		List<MobileProposalView> mobileProposalViews = new ArrayList<MobileProposalView>();
		/*
		 * Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		 * Criteria criteria = session.createCriteria(ProposalView.class);
		 * criteria.add(Restrictions.eq("personId", personId));
		 * criteria.add(Restrictions.eq("certified", false));
		 * criteria.add(Restrictions.eq("statusCode", 1)); List<ProposalView>
		 * proposalViews = criteria.list(); if (proposalViews != null &&
		 * !proposalViews.isEmpty()) { for (ProposalView view : proposalViews) { if
		 * (view.getProposalPersonRoleCode() != null &&
		 * view.getProposalPersonRoleCode().equals("PI")) { MobileProposalView
		 * proposalView = new MobileProposalView();
		 * proposalView.setDocumentNo(view.getDocumentNumber());
		 * proposalView.setTitle(view.getTitle());
		 * proposalView.setLeadUnit(view.getLeadUnit());
		 * proposalView.setProposalNo(view.getProposalNumber());
		 * proposalView.setPi(view.getFullName());
		 * proposalView.setSponsor(view.getSponsor());
		 * proposalView.setPersonId(view.getPersonId());
		 * proposalView.setPersonName(hibernateTemplate.get(PrincipalBo.class,
		 * personId).getPrincipalName());
		 * proposalView.setProposalPersonRoleId(view.getProposalPersonRoleCode());
		 * proposalView.setActionRequestCode("C");
		 * mobileProposalViews.add(proposalView); } } }
		 */
		return mobileProposalViews;
	}

	@Override
	public DashBoardProfile getMobileDashBoardDataForMyProposal(CommonVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		String sortBy = vo.getSortBy();
		String reverse = vo.getReverse();
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		String property3 = vo.getProperty3();
		String property4 = vo.getProperty4();
		String personId = vo.getPersonId();

		Conjunction and = Restrictions.conjunction();
		try {
			logger.info("----------- getMobileDashBoardDataForMyProposal ------------");
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Criteria searchCriteria = session.createCriteria(Proposal.class);

			if (sortBy == null || reverse == null) {
				searchCriteria.addOrder(Order.desc("updateTimeStamp")).addOrder(Order.desc("proposalId"));
			} else {
				if (reverse.equals("DESC")) {
					searchCriteria.addOrder(Order.desc(sortBy)).addOrder(Order.desc("proposalId"));
				} else {
					searchCriteria.addOrder(Order.asc(sortBy)).addOrder(Order.desc("proposalId"));
				}
			}
			if (property1 != null && !property1.isEmpty()) {
				Integer proposalId = Integer.valueOf(property1);
				and.add(Restrictions.like("proposalId", proposalId));
			}
			if (property2 != null && !property2.isEmpty()) {
				and.add(Restrictions.like("title", "%" + property2 + "%").ignoreCase());
			}
			if (property3 != null && !property3.isEmpty()) {
				and.add(Restrictions.like("homeUnitName", "%" + property3 + "%").ignoreCase());
			}
			if (property4 != null && !property4.isEmpty()) {
				and.add(Restrictions.like("sponsorName", "%" + property4 + "%").ignoreCase());
			}
			if (personId != null && !personId.isEmpty()) {
				searchCriteria.createAlias("proposalPersons", "proposalPersons", JoinType.LEFT_OUTER_JOIN);
				searchCriteria.add(Restrictions.disjunction().add(Restrictions.eq("proposalPersons.personId", personId))
						.add(Restrictions.eq("createUser", vo.getUserName()))
						.add(Restrictions.eq("homeUnitNumber", vo.getUnitNumber())));
			}
			searchCriteria.add(and);

			searchCriteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
			@SuppressWarnings("unchecked")
			List<Proposal> proposals = searchCriteria.list();
			List<Proposal> proposalList = new ArrayList<>();
			if (proposals != null && !proposals.isEmpty()) {
				for (Proposal proposalObject : proposals) {
					Proposal propObj = new Proposal();
					propObj.setProposalId(proposalObject.getProposalId());
					propObj.setTitle(proposalObject.getTitle());
					propObj.setApplicationType(proposalObject.getProposalType().getDescription());
					propObj.setApplicationStatus(proposalObject.getProposalStatus().getDescription());
					propObj.setSponsorName(proposalObject.getSponsorName());
					propObj.setHomeUnitName(proposalObject.getHomeUnitName());
					// propObj.setPrincipalInvestigatorForMobile(proposalObject.getPrincipalInvestigator());
					proposalList.add(propObj);
				}
			}
			dashBoardProfile.setProposal(proposalList);
		} catch (Exception e) {
			logger.error("Error in method getMobileDashBoardDataForMyProposal");
			e.printStackTrace();
		}
		return dashBoardProfile;
	}

	/*
	 * private BigDecimal getTotalQuestionnaireCount(Integer proposalId) { Session
	 * session = hibernateTemplate.getSessionFactory().getCurrentSession(); Query
	 * countQuery = session.createSQLQuery(
	 * "select count(1) as qnr_complte_flag from MITKC_QUESTIONNAIRE_ANS_HEADER where MODULE_ITEM_ID = :proposalId"
	 * ); countQuery.setInteger("proposalId", proposalId); return
	 * (BigDecimal)countQuery.list().get(0); }
	 */

	@Override
	public DashBoardProfile getDashBoardDataOfProposalsForMobile(List<Integer> proposalIds) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			logger.info("----------- getDashBoardDataOfProposalsForMobile ------------");
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Criteria searchCriteria = session.createCriteria(Proposal.class);
			List<Integer> proposalStatusCodes = new ArrayList<>();
			proposalStatusCodes.add(Constants.PROPOSAL_STATUS_CODE_SUBMITTED_FOR_REVIEW);
			proposalStatusCodes.add(Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS);
			searchCriteria.add(Restrictions.in("statusCode", proposalStatusCodes));
			searchCriteria.add(Restrictions.in("proposalId", proposalIds));

			searchCriteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
			@SuppressWarnings("unchecked")
			List<Proposal> proposals = searchCriteria.list();
			List<Proposal> proposalList = new ArrayList<>();
			if (proposals != null && !proposals.isEmpty()) {
				for (Proposal proposalObject : proposals) {
					Proposal propObj = new Proposal();
					propObj.setProposalId(proposalObject.getProposalId());
					propObj.setTitle(proposalObject.getTitle());
					propObj.setApplicationType(proposalObject.getProposalType().getDescription());
					propObj.setApplicationStatus(proposalObject.getProposalStatus().getDescription());
					propObj.setSponsorName(proposalObject.getSponsorName());
					propObj.setHomeUnitName(proposalObject.getHomeUnitName());
					// propObj.setPrincipalInvestigatorForMobile(proposalObject.getPrincipalInvestigator());
					proposalList.add(propObj);
				}
			}
			dashBoardProfile.setProposal(proposalList);
		} catch (Exception e) {
			logger.error("Error in method getDashBoardDataOfProposalsForMobile");
			e.printStackTrace();
		}
		return dashBoardProfile;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List<Integer> getApprovalInprogressProposalIdsForMobile(String personId, String approvalStatusCode,
			Integer moduleCode) {
		List<Integer> proposalIds = new ArrayList<Integer>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createSQLQuery(
				"SELECT T1.MODULE_ITEM_ID AS PROPOSAL_ID FROM FIBI_WORKFLOW T1 INNER JOIN FIBI_WORKFLOW_DETAIL T2 ON T1.WORKFLOW_ID = T2.WORKFLOW_ID WHERE T1.MODULE_CODE = :module_code and T1.IS_WORKFLOW_ACTIVE='Y' and T2.APPROVER_PERSON_ID = :person_id and T2.APPROVAL_STATUS_CODE = :approval_status_code");
		query.setString("person_id", personId);
		query.setString("approval_status_code", approvalStatusCode);
		query.setInteger("module_code", moduleCode);
		List<BigDecimal> ids = query.list();
		if (ids != null && !ids.isEmpty()) {
			for (BigDecimal val : ids) {
				proposalIds.add(val.intValue());
			}
		}
		return proposalIds;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List<Integer> getFYIProposalIds(String personId, Integer moduleCode) {
		List<Integer> proposalIds = new ArrayList<Integer>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createSQLQuery(
				"SELECT T1.MODULE_ITEM_ID AS PROPOSAL_ID FROM FIBI_WORKFLOW T1 INNER JOIN FIBI_WORKFLOW_DETAIL T2 ON T1.WORKFLOW_ID = T2.WORKFLOW_ID WHERE T1.MODULE_CODE = :module_code and T1.IS_WORKFLOW_ACTIVE='Y' and T2.APPROVER_PERSON_ID = :person_id AND ((T2.APPROVAL_STATUS_CODE = :approved_by_other_status_code) or (T2.APPROVAL_STATUS_CODE = :approval_by_passed_status_code))");
		query.setString("person_id", personId);
		query.setString("approved_by_other_status_code", Constants.WORKFLOW_STATUS_CODE_APPROVED_BY_OTHER);
		query.setString("approval_by_passed_status_code", Constants.WORKFLOW_STATUS_CODE_APPROVAL_BYPASSED);
		query.setInteger("module_code", moduleCode);
		List<BigDecimal> ids = query.list();
		if (ids != null && !ids.isEmpty()) {
			for (BigDecimal val : ids) {
				proposalIds.add(val.intValue());
			}
		}
		return proposalIds;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List<Integer> getCertificationInCompleteProposalIds(String personId) {
		List<Integer> proposalIds = new ArrayList<Integer>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createSQLQuery(
				"SELECT T1.PROPOSAL_ID FROM EPS_PROPOSAL T1 INNER JOIN MITKC_QUESTIONNAIRE_ANS_HEADER T2 ON T1.PROPOSAL_ID = T2.MODULE_ITEM_ID AND T2.MODULE_ITEM_CODE = :module_code WHERE T1.STATUS_CODE = :status_code AND (T1.HOME_UNIT_NUMBER IN( SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME = 'VIEW_PROPOSAL' AND PERSON_ID = :PERSON_ID) OR T1.PROPOSAL_ID IN ( SELECT T1.PROPOSAL_ID FROM EPS_PROPOSAL_PERSONS T1 WHERE T1.PERSON_ID = :person_id AND T1.PROP_PERSON_ROLE_ID IN (1,2,3))) AND T2.QUESTIONNAIRE_COMPLETED_FLAG = :questionnaire_complete_flag");
		query.setString("person_id", personId);
		query.setString("questionnaire_complete_flag", Constants.QUESTIONNAIRE_INCOMPLETE_FLAG);
		query.setInteger("module_code", 3);
		query.setInteger("status_code", Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS);
		List<BigDecimal> ids = query.list();
		if (ids != null && !ids.isEmpty()) {
			for (BigDecimal val : ids) {
				proposalIds.add(val.intValue());
			}
		}
		return proposalIds;
	}

	@Override
	public DashBoardProfile getDashBoardDataOfUncertifiedProposals(List<Integer> proposalIds) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			logger.info("----------- getDashBoardDataOfUncertifiedProposals ------------");
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Criteria searchCriteria = session.createCriteria(Proposal.class);
			searchCriteria.add(Restrictions.in("proposalId", proposalIds));

			searchCriteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
			@SuppressWarnings("unchecked")
			List<Proposal> proposals = searchCriteria.list();
			List<Proposal> proposalList = new ArrayList<>();
			if (proposals != null && !proposals.isEmpty()) {
				for (Proposal proposalObject : proposals) {
					Proposal propObj = new Proposal();
					propObj.setProposalId(proposalObject.getProposalId());
					propObj.setTitle(proposalObject.getTitle());
					if (proposalObject.getActivityType() != null) {
						propObj.setApplicationActivityType(proposalObject.getActivityType().getDescription());
					}
					if (proposalObject.getProposalType() != null) {
						propObj.setApplicationType(proposalObject.getProposalType().getDescription());
					}
					propObj.setApplicationStatus("C");
					propObj.setSubmissionDate(proposalObject.getSubmissionDate());
					propObj.setSponsorName(proposalObject.getSponsorName());
					propObj.setHomeUnitName(proposalObject.getHomeUnitName());
					propObj.setSubmitUser(proposalObject.getSubmitUser());
					// propObj.setPrincipalInvestigatorForMobile(proposalObject.getPrincipalInvestigator());
					proposalList.add(propObj);
				}
			}
			dashBoardProfile.setProposal(proposalList);
		} catch (Exception e) {
			logger.error("Error in method getDashBoardDataOfUncertifiedProposals");
			e.printStackTrace();
		}
		return dashBoardProfile;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public DashBoardProfile getProposalsInProgressForMobile(String personId, boolean isAdmin, String unitNumber)
			throws Exception {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<Proposal> inProgressProposals = new ArrayList<Proposal>();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query progressProposalList = null;
			if (isAdmin) {
				if (unitNumber != null) {
					progressProposalList = session.createSQLQuery(
							"SELECT T1.PROPOSAL_ID, T1.TITLE, T5.SPONSOR_NAME, T2.TOTAL_COST, T4.FULL_NAME AS PI, T1.HOME_UNIT_NUMBER AS UNIT_NUMBER, T6.UNIT_NAME, T1.SUBMISSION_DATE FROM EPS_PROPOSAL T1 LEFT OUTER JOIN BUDGET_HEADER T2 ON T1.BUDGET_HEADER_ID=T2.BUDGET_HEADER_ID LEFT OUTER JOIN EPS_PROPOSAL_PERSONS T4 ON T1.PROPOSAL_ID = T4.PROPOSAL_ID AND T4.PROP_PERSON_ROLE_ID = 3 INNER JOIN SPONSOR T5 ON T1.SPONSOR_CODE = T5.SPONSOR_CODE INNER JOIN UNIT T6 ON T1.HOME_UNIT_NUMBER= T6.UNIT_NUMBER WHERE T1.STATUS_CODE=1 AND (T1.HOME_UNIT_NUMBER IN( SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME ='VIEW_PROPOSAL' AND PERSON_ID = :personId AND UNIT_NUMBER = :unitNumber) OR T1.PROPOSAL_ID IN ( SELECT T1.PROPOSAL_ID FROM EPS_PROPOSAL_PERSONS T1 INNER JOIN EPS_PROP_PERSON_UNITS T2 ON T1.PROPOSAL_PERSON_ID = T2.PROPOSAL_PERSON_ID WHERE T1.PERSON_ID = :personId AND T1.PROP_PERSON_ROLE_ID IN (1,2,3) AND T2.UNIT_NUMBER = :unitNumber))");
					progressProposalList.setString("unitNumber", unitNumber);
				} else {
					progressProposalList = session.createSQLQuery(
							"SELECT T1.PROPOSAL_ID, T1.TITLE, T5.SPONSOR_NAME, T2.DESCRIPTION AS STATUS, T4.FULL_NAME AS PI, T1.HOME_UNIT_NUMBER AS UNIT_NUMBER, T6.UNIT_NAME, T3.DESCRIPTION AS PROPOSAL_TYPE FROM EPS_PROPOSAL T1 LEFT OUTER JOIN EPS_PROPOSAL_STATUS T2 ON T1.STATUS_CODE = T2.STATUS_CODE LEFT OUTER JOIN EPS_PROPOSAL_TYPE T3 ON T1.TYPE_CODE = T3.TYPE_CODE LEFT OUTER JOIN EPS_PROPOSAL_PERSONS T4 ON T1.PROPOSAL_ID = T4.PROPOSAL_ID AND T4.PROP_PERSON_ROLE_ID = 3 INNER JOIN SPONSOR T5 ON T1.SPONSOR_CODE = T5.SPONSOR_CODE INNER JOIN UNIT T6 ON T1.HOME_UNIT_NUMBER= T6.UNIT_NUMBER WHERE T1.STATUS_CODE = 1 AND (T1.HOME_UNIT_NUMBER IN( SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME ='VIEW_PROPOSAL' and person_id = :personId) OR T1.PROPOSAL_ID IN ( SELECT T1.PROPOSAL_ID FROM EPS_PROPOSAL_PERSONS T1 WHERE T1.PERSON_ID = :PERSONID AND T1.PROP_PERSON_ROLE_ID IN (1,2,3)))");
				}
			} else {
				progressProposalList = session.createSQLQuery(
						"SELECT T1.PROPOSAL_ID, T1.TITLE, T5.SPONSOR_NAME, T2.DESCRIPTION AS STATUS, T4.FULL_NAME AS PI, T1.HOME_UNIT_NUMBER AS UNIT_NUMBER, T6.UNIT_NAME, T3.DESCRIPTION AS PROPOSAL_TYPE FROM EPS_PROPOSAL T1 LEFT OUTER JOIN EPS_PROPOSAL_STATUS T2 ON T1.STATUS_CODE = T2.STATUS_CODE LEFT OUTER JOIN EPS_PROPOSAL_TYPE T3 ON T1.TYPE_CODE = T3.TYPE_CODE LEFT OUTER JOIN EPS_PROPOSAL_PERSONS T4 ON T1.PROPOSAL_ID = T4.PROPOSAL_ID AND T4.PROP_PERSON_ROLE_ID = 3 INNER JOIN SPONSOR T5 ON T1.SPONSOR_CODE = T5.SPONSOR_CODE INNER JOIN UNIT T6 ON T1.HOME_UNIT_NUMBER= T6.UNIT_NUMBER WHERE T1.STATUS_CODE=1 AND T1.HOME_UNIT_NUMBER IN( SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT where RIGHT_NAME ='VIEW_PROPOSAL' AND PERSON_ID = :personId)");
			}
			progressProposalList.setString("personId", personId);
			List<Object[]> proposals = progressProposalList.list();
			for (Object[] proposal : proposals) {
				Proposal proposalObj = new Proposal();
				proposalObj.setProposalId(Integer.valueOf(proposal[0].toString()));
				proposalObj.setTitle(proposal[1].toString());
				proposalObj.setSponsorName(proposal[2].toString());
				proposalObj.setApplicationStatus(proposal[3].toString());
				proposalObj.setPrincipalInvestigatorForMobile(proposal[4].toString());
				proposalObj.setHomeUnitName(proposal[6].toString());
				proposalObj.setApplicationType(proposal[7].toString());
				inProgressProposals.add(proposalObj);
			}
			logger.info("ProposalsInProgressForMobile : " + inProgressProposals);
			dashBoardProfile.setProposal(inProgressProposals);
		} catch (Exception e) {
			logger.error("Error in method getProposalsInProgressForMobile");
			e.printStackTrace();
		}
		return dashBoardProfile;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public DashBoardProfile getSubmittedProposalsForMobile(String personId, boolean isAdmin, String unitNumber)
			throws Exception {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<Proposal> submittedProposals = new ArrayList<Proposal>();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query subproposalList = null;
			if (isAdmin) {
				if (unitNumber != null) {
					subproposalList = session.createSQLQuery(
							"SELECT T1.PROPOSAL_ID, T1.TITLE, T5.SPONSOR_NAME, T2.TOTAL_COST, T4.FULL_NAME AS PI, T1.HOME_UNIT_NUMBER AS UNIT_NUMBER, T6.UNIT_NAME, T1.SUBMISSION_DATE FROM EPS_PROPOSAL T1 LEFT OUTER JOIN BUDGET_HEADER T2 ON T1.BUDGET_HEADER_ID=T2.BUDGET_HEADER_ID LEFT OUTER JOIN EPS_PROPOSAL_PERSONS T4 ON T1.PROPOSAL_ID = T4.PROPOSAL_ID AND T4.PROP_PERSON_ROLE_ID = 3 INNER JOIN SPONSOR T5 ON T1.SPONSOR_CODE = T5.SPONSOR_CODE INNER JOIN UNIT T6 ON  T1.HOME_UNIT_NUMBER= T6.UNIT_NUMBER WHERE T1.STATUS_CODE=2 AND (T1.HOME_UNIT_NUMBER IN( SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME ='VIEW_PROPOSAL' AND PERSON_ID = :personId AND UNIT_NUMBER = :unitNumber) OR T1.PROPOSAL_ID IN ( SELECT T1.PROPOSAL_ID FROM EPS_PROPOSAL_PERSONS T1 INNER JOIN EPS_PROP_PERSON_UNITS T2 ON T1.PROPOSAL_PERSON_ID = T2.PROPOSAL_PERSON_ID WHERE T1.PERSON_ID = :personId AND T1.PROP_PERSON_ROLE_ID IN (1,2,3) AND T2.UNIT_NUMBER = :unitNumber))");
					subproposalList.setString("unitNumber", unitNumber);
				} else {
					subproposalList = session.createSQLQuery(
							"SELECT T1.PROPOSAL_ID, T1.TITLE, T5.SPONSOR_NAME, T2.DESCRIPTION AS STATUS, T4.FULL_NAME AS PI, T1.HOME_UNIT_NUMBER AS UNIT_NUMBER, T6.UNIT_NAME, T3.DESCRIPTION AS PROPOSAL_TYPE FROM EPS_PROPOSAL T1 LEFT OUTER JOIN EPS_PROPOSAL_STATUS T2 ON T1.STATUS_CODE = T2.STATUS_CODE LEFT OUTER JOIN EPS_PROPOSAL_TYPE T3 ON T1.TYPE_CODE = T3.TYPE_CODE LEFT OUTER JOIN EPS_PROPOSAL_PERSONS T4 ON T1.PROPOSAL_ID = T4.PROPOSAL_ID AND T4.PROP_PERSON_ROLE_ID = 3 INNER JOIN SPONSOR T5 ON T1.SPONSOR_CODE = T5.SPONSOR_CODE INNER JOIN UNIT T6 ON  T1.HOME_UNIT_NUMBER= T6.UNIT_NUMBER WHERE T1.STATUS_CODE=2 AND (T1.HOME_UNIT_NUMBER IN( SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME ='VIEW_PROPOSAL' AND PERSON_ID = :personId) OR T1.PROPOSAL_ID IN ( SELECT T1.PROPOSAL_ID FROM EPS_PROPOSAL_PERSONS T1 WHERE T1.PERSON_ID = :personId AND T1.PROP_PERSON_ROLE_ID IN (1,2,3)))");
				}
			} else {
				subproposalList = session.createSQLQuery(
						"SELECT T1.PROPOSAL_ID, T1.TITLE, T5.SPONSOR_NAME, T2.DESCRIPTION AS STATUS, T4.FULL_NAME AS PI, T1.HOME_UNIT_NUMBER AS UNIT_NUMBER, T6.UNIT_NAME, T3.DESCRIPTION AS PROPOSAL_TYPE FROM EPS_PROPOSAL T1 LEFT OUTER JOIN EPS_PROPOSAL_STATUS T2 ON T1.STATUS_CODE = T2.STATUS_CODE LEFT OUTER JOIN EPS_PROPOSAL_TYPE T3 ON T1.TYPE_CODE = T3.TYPE_CODE LEFT OUTER JOIN EPS_PROPOSAL_PERSONS T4 ON T1.PROPOSAL_ID = T4.PROPOSAL_ID AND T4.PROP_PERSON_ROLE_ID = 3 INNER JOIN SPONSOR T5 ON T1.SPONSOR_CODE = T5.SPONSOR_CODE INNER JOIN UNIT T6 ON  T1.HOME_UNIT_NUMBER= T6.UNIT_NUMBER WHERE T1.STATUS_CODE=2 AND T1.HOME_UNIT_NUMBER IN( SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME ='VIEW_PROPOSAL' AND PERSON_ID = :personId)");
			}
			subproposalList.setString("personId", personId);
			List<Object[]> subProposals = subproposalList.list();
			for (Object[] proposal : subProposals) {
				Proposal proposalObj = new Proposal();
				proposalObj.setProposalId(Integer.valueOf(proposal[0].toString()));
				proposalObj.setTitle(proposal[1].toString());
				proposalObj.setSponsorName(proposal[2].toString());
				proposalObj.setApplicationStatus(proposal[3].toString());
				proposalObj.setPrincipalInvestigatorForMobile(proposal[4].toString());
				proposalObj.setHomeUnitName(proposal[6].toString());
				proposalObj.setApplicationType(proposal[7].toString());
				submittedProposals.add(proposalObj);
			}
			logger.info("SubmittedProposalsForMobile : " + submittedProposals);
			dashBoardProfile.setProposal(submittedProposals);
		} catch (Exception e) {
			logger.error("Error in method getSubmittedProposalsForMobile");
			e.printStackTrace();
		}
		return dashBoardProfile;
	}

}
