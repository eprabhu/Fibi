package com.polus.fibicomp.proposal.lookup.dao;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import com.polus.fibicomp.proposal.pojo.*;
import org.apache.commons.collections4.ListUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibility;
import com.polus.fibicomp.pojo.ActivityType;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.pojo.ScienceKeyword;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.roles.dao.RolesManagementDao;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.vo.SponsorSearchResult;

@Transactional
@Service(value = "proposalLookUpDao")
public class ProposalLookUpDaoImpl implements ProposalLookUpDao {

	protected static Logger logger = LogManager.getLogger(ProposalLookUpDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private RolesManagementDao rolesManagementDao;

	@Autowired
	public CommonDao commonDao;

	private static final String UNIT_NAME = "unitName";
	private static final String UNIT_NUMBER = "unitNumber";
	private static final String DESCRIPTION = "description";
	private static final String ACTIVE = "active";
	private static final String GRANT_STATUS_CODE = "grantStatusCode";

	@SuppressWarnings("unchecked")
	@Override
	public List<SponsorSearchResult> findSponsor(String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		final String likeCriteria = "%" + searchString.toUpperCase() + "%";
		org.hibernate.query.Query<SponsorSearchResult> query = session.createQuery("SELECT NEW com.polus.fibicomp.vo.SponsorSearchResult(t.sponsorCode, t.sponsorName, c.countryCode, c.countryName, t.acronym, t.sponsorTypeCode, s.description, t.addressLine1, t.addressLine2, t.addressLine3, t.state, t.postalCode, t.sponsorLocation) "
						+ "FROM Sponsor t left outer join Country c on c.countryCode = t.countryCode "
						+ " left outer join SponsorType s on s.code = t.sponsorTypeCode "
						+ "WHERE (UPPER(t.sponsorCode) like :likeCriteria OR UPPER(t.acronym) like :likeCriteria or UPPER(t.sponsorName) like :likeCriteria) and t.active = 'Y'");
		query.setParameter("likeCriteria", likeCriteria);
		return ListUtils.emptyIfNull(query.setMaxResults(25).list());
	}

	@Override
	public List<GrantCall> getGrantCallsBasedOnSearchString(String searchString, Integer moduleCode, Boolean includeClosedGrantCall, Boolean isExternalUser) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCall> grantCallCriteria = builder.createQuery(GrantCall.class);
		Root<GrantCall> rootGrantCall = grantCallCriteria.from(GrantCall.class);
		Predicate status = null;
		Predicate grantName = builder.like(builder.lower(rootGrantCall.get("grantCallName")), "%" + searchString.toLowerCase() + "%");
		Predicate grantId = builder.like(builder.lower(rootGrantCall.get("grantCallId").as(String.class)), "%" + searchString.toLowerCase() + "%");
		Predicate externalGrantCall = null;
		Boolean enableClosedGrantCall = commonDao.getParameterValueAsBoolean(Constants.ENABLE_CLOSED_GRANTCALL_LINKING_IN_PROPOSAL);
		List<Integer> grantCallStatusForAward = new ArrayList<>();
		grantCallStatusForAward.add(Constants.GRANT_CALL_STATUS_CODE_NOT_TO_PUBLISH);
		grantCallStatusForAward.add(Constants.GRANT_CALL_STATUS_CODE_OPEN);
		List<Integer> grantCallStatusForProposal = new ArrayList<>();
		grantCallStatusForProposal.add(Constants.GRANT_CALL_STATUS_CODE_OPEN);
		if (Boolean.TRUE.equals(isExternalUser)) {
			Join<GrantCall, GrantCallEligibility> join = rootGrantCall.join("grantCallEligibility");
			externalGrantCall = builder.equal(join.get("grantEligibilityTypeCode"), Constants.EXTERNAL_GRANT_ELIGIBILITY_TYPE_CODE);
		}
		if (moduleCode.equals(Constants.AWARD_MODULE_CODE)) {
			if (Boolean.TRUE.equals(enableClosedGrantCall)) {
				grantCallStatusForAward.add(Constants.GRANT_CALL_STATUS_CODE_CLOSED);
			} 
			status = rootGrantCall.get(GRANT_STATUS_CODE).in(grantCallStatusForAward);
			if (Boolean.TRUE.equals(isExternalUser)) {
				grantCallCriteria.where(builder.or(builder.and(status, grantName, externalGrantCall), builder.and(status, grantId, externalGrantCall)));
			} else {
				grantCallCriteria.where(builder.or(builder.and(status, grantName), builder.and(status, grantId)));
			}
		} else if (moduleCode.equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
			if (Boolean.TRUE.equals(enableClosedGrantCall)) {
				grantCallStatusForProposal.add(Constants.GRANT_CALL_STATUS_CODE_CLOSED);
			}
			status = rootGrantCall.get(GRANT_STATUS_CODE).in(grantCallStatusForProposal);
			Predicate isPublish = builder.equal(rootGrantCall.get("isPublished"), true);
			Predicate publishGrantCall = builder.or(builder.and(status, grantName, isPublish), builder.and(status, isPublish, grantId));
			if (Boolean.TRUE.equals(includeClosedGrantCall)) {
				Predicate notPublishGrantCall = builder.or(builder.and(status, grantName), builder.and(status, grantId));
				if (Boolean.TRUE.equals(isExternalUser)) {
					grantCallCriteria.where(builder.or(publishGrantCall, notPublishGrantCall, externalGrantCall));
				} else {
					grantCallCriteria.where(builder.or(publishGrantCall, notPublishGrantCall));
				}
			} else {
				if (Boolean.TRUE.equals(isExternalUser)) {
					grantCallCriteria.where(builder.and(publishGrantCall, externalGrantCall, status));
				} else {
					grantCallCriteria.where(builder.and(publishGrantCall, status));
				}
			}
		}
		grantCallCriteria.orderBy(builder.asc(rootGrantCall.get("grantCallName")));
		grantCallCriteria.distinct(true);
		List<GrantCall> selectedGrantCalls = session.createQuery(grantCallCriteria).setMaxResults(25).getResultList();
		List<GrantCall> grantCalls = new ArrayList<>();
		for(GrantCall grantCallData : selectedGrantCalls) {
			GrantCall grantCall = new GrantCall();
			grantCall.setGrantCallId(grantCallData.getGrantCallId());
			grantCall.setGrantCallName(grantCallData.getGrantCallName());
			if (grantCallData.getSponsorCode() != null) {
				Sponsor sponsor = commonDao.getSponsorById(grantCallData.getSponsorCode());
				if (sponsor != null) {
					String sponsorAcronym = sponsor.getAcronym() != null ? " (" + sponsor.getAcronym() + ")" : "";
					grantCall.setSponsorName(sponsor.getSponsorCode() + " - " + sponsor.getSponsorName() + sponsorAcronym);
				}
			}
			grantCall.setSponsorCode(grantCallData.getSponsorCode());
			grantCall.setGrantCallType(grantCallData.getGrantCallType());
			grantCall.setGrantCallStatus(grantCallData.getGrantCallStatus());
			grantCall.setClosingDate(grantCallData.getClosingDate());
			grantCall.setInternalSubmissionDeadLineDate(grantCallData.getInternalSubmissionDeadLineDate());
			grantCall.setHomeUnitName(grantCallData.getHomeUnitName());
			grantCall.setHomeUnitNumber(grantCallData.getHomeUnitNumber());
			grantCall.setAbbrevation(grantCallData.getAbbrevation());
			if (grantCallData.getPrimeSponsorCode() != null) {
				grantCall.setPrimeSponsorCode(grantCallData.getPrimeSponsorCode());
				grantCall.setPrimeSponsor(commonDao.getSponsorById(grantCallData.getPrimeSponsorCode()));
			}
			if(grantCallData.getSponsorFundingScheme() != null) {
				grantCall.setSponsorFundingScheme(grantCallData.getSponsorFundingScheme());
			}
			grantCalls.add(grantCall);
		}
		return grantCalls;
	}

	@Override
	public List<Unit> getDepartmentList(String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Unit> query = builder.createQuery(Unit.class);
		Root<Unit> rootUnit = query.from(Unit.class);
		Predicate unitName = builder.like(builder.lower(rootUnit.get(UNIT_NAME)), "%" + searchString.toLowerCase() + "%");
		Predicate unitNumber = builder.like(rootUnit.get(UNIT_NUMBER), "%" + searchString + "%");
		Predicate isActive = builder.equal(rootUnit.get(ACTIVE), true);
		query.where(builder.or(unitName, unitNumber), builder.and(isActive));
		query.select(builder.construct(Unit.class, rootUnit.get(UNIT_NUMBER), rootUnit.get(UNIT_NAME)));
		query.orderBy(builder.asc(rootUnit.get(UNIT_NAME)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<CostElement> findCostElementByParams(String searchString, List<String> budgetCategoryCodes) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CostElement> query = builder.createQuery(CostElement.class);
		Root<CostElement> rootCostElement = query.from(CostElement.class);
		Predicate predicateBudgetCategoryCode = null;
		if (budgetCategoryCodes != null && !budgetCategoryCodes.isEmpty()) {
			predicateBudgetCategoryCode = builder.not(rootCostElement.get("budgetCategoryCode").in(budgetCategoryCodes));
		}
		Predicate predicateDescription = builder.like(builder.lower(rootCostElement.get(DESCRIPTION)), "%" + searchString.toLowerCase() + "%");
		Predicate predicateCostElement = builder.like(builder.lower(rootCostElement.get("costElement")), "%" + searchString.toLowerCase() + "%");
		Predicate predicate = builder.or(predicateDescription, predicateCostElement);
		Predicate predicateActiveFlag = builder.equal(rootCostElement.get(ACTIVE), true);
		if (predicateBudgetCategoryCode != null) {
			query.where(builder.and(predicateBudgetCategoryCode, predicate, predicateActiveFlag));
		} else {
			query.where(builder.and(predicate, predicateActiveFlag));
		}
		return session.createQuery(query).setMaxResults(25).getResultList();
	}


	@Override
	public List<ScienceKeyword> findKeyWordsList(String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ScienceKeyword> query = builder.createQuery(ScienceKeyword.class);
		Root<ScienceKeyword> rootScienceKeyword = query.from(ScienceKeyword.class);
		Predicate isActive = builder.equal(rootScienceKeyword.get("isActive"), Boolean.TRUE);
		Predicate description = builder.like(builder.lower(rootScienceKeyword.get(DESCRIPTION)), "%" + searchString.toLowerCase() + "%");
		query.where(builder.and(isActive, description));
		query.orderBy(builder.asc(rootScienceKeyword.get(Constants.DESCRIPTION)));
		return session.createQuery(query).setMaxResults(25).getResultList();
	}

	@Override
	public List<Unit> fetchLeadUnitsByUnitNumbers(Set<String> unitNumbers, String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Unit> query = builder.createQuery(Unit.class);
		Root<Unit> rootUnit = query.from(Unit.class);
		Predicate predicate1 = rootUnit.get(UNIT_NUMBER).in(unitNumbers);
		Predicate predicate2 = builder.equal(rootUnit.get(ACTIVE), true);
		Predicate predicate3 = builder.like(builder.lower(rootUnit.get(UNIT_NAME)), "%" + searchString.toLowerCase() + "%");
		query.where(builder.and(predicate1, predicate2, predicate3));
		query.select(builder.construct(Unit.class, rootUnit.get(UNIT_NUMBER), rootUnit.get(UNIT_NAME)));
		query.orderBy(builder.asc(rootUnit.get(UNIT_NAME)));
		return session.createQuery(query).setMaxResults(25).getResultList();
	}

	@Override
	public List<BudgetCategory> findBudgetCategoryList(String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetCategory> query = builder.createQuery(BudgetCategory.class);
		Root<BudgetCategory> rootBudgetCategory = query.from(BudgetCategory.class);
		query.where(builder.like(builder.lower(rootBudgetCategory.get(DESCRIPTION)), "%" + searchString.toLowerCase() + "%"));
		query.orderBy(builder.asc(rootBudgetCategory.get(Constants.DESCRIPTION)));
		return session.createQuery(query).setMaxResults(25).getResultList();
	}

	@Override
	public List<ProposalAttachmentType> fetchAllProposalAttachmentTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();		
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalAttachmentType> query = builder.createQuery(ProposalAttachmentType.class);
		Root<ProposalAttachmentType> attachmentType = query.from(ProposalAttachmentType.class);
		query.orderBy(builder.asc(attachmentType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProposalPersonRole> fetchAllProposalPersonRoles() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();		
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPersonRole> query = builder.createQuery(ProposalPersonRole.class);
		Root<ProposalPersonRole> rootProposalPersonRole = query.from(ProposalPersonRole.class);
		query.orderBy(builder.asc(rootProposalPersonRole.get("sortId")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ProposalStatus fetchProposalStatusByStatusCode(Integer statusCode) {
		return hibernateTemplate.get(ProposalStatus.class, statusCode);
	}

	@Override
	public List<ProposalType> fetchAllProposalTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalType> query = builder.createQuery(ProposalType.class);
		Root<ProposalType> proposalType = query.from(ProposalType.class);
		query.orderBy(builder.asc(proposalType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProposalPersonRoles> fetchProposalPersonRoles(Integer proposalId, Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPersonRoles> query = builder.createQuery(ProposalPersonRoles.class);
		Root<ProposalPersonRoles> rootProposalPersonRoles = query.from(ProposalPersonRoles.class);
		Predicate predicateOne = builder.equal(rootProposalPersonRoles.get("proposalId"), proposalId);
		Predicate predicateTwo = builder.equal(rootProposalPersonRoles.get("roleId"), roleId);
		if (roleId != null) {
			query.where(builder.and(predicateOne, predicateTwo));
		} else {
			query.where(builder.and(predicateOne));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Role> fetchProposalRoles() {
		List<Integer> roleList = new ArrayList<>();
		roleList.add(Constants.PROPOSAL_AGGREGATOR_ROLE_ID);
		roleList.add(Constants.PROPOSAL_BUDGET_CREATOR_ROLE_ID);
		roleList.add(Constants.VIEW_PROPOSAL_ROLE_ID);
		roleList.add(Constants.REVIEW_PROPOSAL_ROLE_ID);
		return rolesManagementDao.getRoleBasedOnRoleId(roleList);
	}

	@Override
	public List<DisciplineCluster> fetchAllDisciplineCluster() {
		return hibernateTemplate.loadAll(DisciplineCluster.class);
	}

	@Override
	public List<ProposalFundingStatus> fetchAllProposalFundingStatus() {
		return hibernateTemplate.loadAll(ProposalFundingStatus.class);
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ActivityType> getAllActivityForGrantType() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery(" select t1.activityTypeCode, t1.description, t2.grantTypeCode, t2.grantCallType.categoryCode, t1.isActive" + " from ActivityType t1 inner join ActivityGrantCallMapping t2 on t1.activityTypeCode = t2.activityTypeCode order by t1.description asc");
		List<Object[]> result = query.getResultList();
		return setActivityType(result);
	}
	
	private List<ActivityType> setActivityType(List<Object[]> result) {
		List<ActivityType> activityTypes = new ArrayList<>();
		for (Object[] obj : result) {
			ActivityType activityType = new ActivityType();
			activityType.setActivityTypeCode(obj[0].toString());
			activityType.setDescription(obj[1].toString());
			activityType.setGrantTypeCode(Integer.parseInt((String) obj[2]));
			activityType.setCategoryCode(Integer.parseInt(obj[3].toString()));
			activityType.setIsActive((Boolean) obj[4]);
			activityTypes.add(activityType);
		}
		return activityTypes;
	}

	@Override
	public List<ProposalStatus> fetAllProposalStatus(List<Integer> statusCodes) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalStatus> query = builder.createQuery(ProposalStatus.class);
		Root<ProposalStatus> rootProposalStatus = query.from(ProposalStatus.class);
		if (statusCodes != null && !statusCodes.isEmpty()) {
			query.where((rootProposalStatus.get("statusCode").in(statusCodes)));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<OrganizationType> loadOrganizationType() {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<OrganizationType> criteria = builder.createQuery(OrganizationType.class);
			Root<OrganizationType> milestoneStatuses = criteria.from(OrganizationType.class);
			criteria.where(builder.equal(milestoneStatuses.get("isActive"), Boolean.TRUE));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<ProposalKeyPersonnelAttachmentType> fetchAllKeyPersonalProposalAttachmentTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalKeyPersonnelAttachmentType> query = builder.createQuery(ProposalKeyPersonnelAttachmentType.class);
		Root<ProposalKeyPersonnelAttachmentType> attachmentType = query.from(ProposalKeyPersonnelAttachmentType.class);
		Predicate attachmentTypeActive = builder.equal(attachmentType.get("isActive"), true);
		query.where(builder.and(attachmentTypeActive));
		query.orderBy(builder.asc(attachmentType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<OrganizationType> loadAllOrganizationType() {
		return hibernateTemplate.loadAll(OrganizationType.class);
	}
}
