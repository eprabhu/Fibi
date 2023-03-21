package com.polus.fibicomp.budget.dao;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.adminportal.pojo.InstituteRate;
import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.budget.pojo.AppointmentType;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetPeriod;
import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.BudgetDetail;
import com.polus.fibicomp.budget.pojo.BudgetDetailCalcAmount;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetPeriod;
import com.polus.fibicomp.budget.pojo.BudgetPerson;
import com.polus.fibicomp.budget.pojo.BudgetPersonalDetails;
import com.polus.fibicomp.budget.pojo.BudgetStatus;
import com.polus.fibicomp.budget.pojo.BudgetTemplate;
import com.polus.fibicomp.budget.pojo.BudgetTemplateType;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.budget.pojo.FibiProposalRate;
import com.polus.fibicomp.budget.pojo.JobCode;
import com.polus.fibicomp.budget.pojo.TbnPerson;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.proposal.pojo.CostSharingType;
import com.polus.fibicomp.budget.pojo.FundDisbursementBasisType;

@Transactional
@Service(value = "budgetDao")
public class BudgetDaoImpl implements BudgetDao {

	protected static Logger logger = LogManager.getLogger(BudgetDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	private static final String ACTIVITYTYPECODE = "activityTypeCode";
	private static final String PROPOSALID = "proposalId";
	private static final String STARTDATE = "startDate";
	private static final String RATECLASSCODE = "rateClassCode";
	private static final String RATETYPECODE = "rateTypeCode";
	private static final String BUDGETPERSONID = "budgetPersonId";

	@Autowired
	public CommonDao commonDao;

	@Override
	public List<InstituteRate> filterInstituteRateByDateRange(Timestamp startDate, Timestamp endDate, String activityTypeCode, String campusFlag) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteRate> query = builder.createQuery(InstituteRate.class);
		Root<InstituteRate> rootInstituteRate = query.from(InstituteRate.class);
		Predicate predicateActivityTypeCode = builder.equal(rootInstituteRate.get(ACTIVITYTYPECODE), activityTypeCode);
		Predicate predicateCampusFlag = builder.equal(rootInstituteRate.get("onOffCampusFlag"), campusFlag);
		Predicate predicateDatePeriod = builder.between(rootInstituteRate.get(STARTDATE), startDate, endDate);
		query.where(builder.and(predicateActivityTypeCode, predicateCampusFlag, predicateDatePeriod));
		query.orderBy(builder.asc(rootInstituteRate.get(STARTDATE)));
		return session.createQuery(query).list();
	}

	@Override
	public List<CostElement> getAllCostElements() {
		return hibernateTemplate.loadAll(CostElement.class);
	}

	@Override
	public RateType getOHRateTypeByParams(String rateClassCode, String rateTypeCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<RateType> query = builder.createQuery(RateType.class);
			Root<RateType> rootRateType = query.from(RateType.class);
			Predicate predicateRateClassCode = builder.equal(rootRateType.get(RATECLASSCODE), rateClassCode);
			Predicate predicateRateTypeCode = builder.equal(rootRateType.get(RATETYPECODE), rateTypeCode);
			query.where(builder.and(predicateRateClassCode, predicateRateTypeCode));
			return session.createQuery(query).uniqueResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public FibiProposalRate fetchApplicableProposalRate(Integer budgetId, Timestamp budgetStartDate, String rateClassCode, String rateTypeCode, String activityTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		FibiProposalRate proposalRate = null;
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<FibiProposalRate> query = builder.createQuery(FibiProposalRate.class);
		Root<FibiProposalRate> rootFibiProposalRate = query.from(FibiProposalRate.class);
		Predicate predicateBudgetId = builder.equal(rootFibiProposalRate.get("budgetHeader").get("budgetId"), budgetId);
		Predicate predicateStartDate = builder.lessThanOrEqualTo(rootFibiProposalRate.get(STARTDATE), budgetStartDate);
		Predicate predicateRateClassCOde = builder.equal(rootFibiProposalRate.get(RATECLASSCODE), rateClassCode);
		Predicate predicateRateTypeCode = builder.equal(rootFibiProposalRate.get(RATETYPECODE), rateTypeCode);
		Predicate predicateActivityTypeCode = builder.equal(rootFibiProposalRate.get(ACTIVITYTYPECODE), activityTypeCode);
		query.where(builder.and(predicateBudgetId, predicateStartDate, predicateRateClassCOde, predicateRateTypeCode, predicateActivityTypeCode));
		query.orderBy(builder.desc(rootFibiProposalRate.get(STARTDATE)));
		List<FibiProposalRate> fibiProposalRates = session.createQuery(query).list();
		if (fibiProposalRates != null && !fibiProposalRates.isEmpty()) {
			proposalRate = fibiProposalRates.get(0);
		}
		return proposalRate;
	}

	@Override
	public BudgetHeader fetchBudgetByBudgetId(Integer budgetId) {
		return hibernateTemplate.get(BudgetHeader.class, budgetId);
	}

	@Override
	public void saveOrUpdateBudget(BudgetHeader budgetHeader) {
		hibernateTemplate.merge(budgetHeader);
	}

	@Override
	public InstituteRate fetchInstituteRateByDateLessthanMax(Timestamp startDate, String activityTypeCode, String rateClassCode, String rateTypeCode) {		
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		InstituteRate instituteRate = null;
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteRate> query = builder.createQuery(InstituteRate.class);
		Root<InstituteRate> rootInstituteRate = query.from(InstituteRate.class);
		Predicate predicateActivityTypeCode = builder.equal(rootInstituteRate.get(ACTIVITYTYPECODE), activityTypeCode);
		Predicate predicateRateClassCOde = builder.equal(rootInstituteRate.get(RATECLASSCODE), rateClassCode);
		Predicate predicateRateTypeCode = builder.equal(rootInstituteRate.get(RATETYPECODE), rateTypeCode);
		Predicate predicateStartDate = builder.lessThan(rootInstituteRate.get(STARTDATE), startDate);
		query.where(builder.and(predicateActivityTypeCode, predicateRateClassCOde, predicateRateTypeCode, predicateStartDate));
		query.orderBy(builder.desc(rootInstituteRate.get(STARTDATE)));
		query.distinct(true);
		List<InstituteRate> instituteRates = session.createQuery(query).list();
		if (instituteRates != null && !instituteRates.isEmpty()) {
			instituteRate = instituteRates.get(0);
		}
		return instituteRate;
	}

	@Override
	public BudgetPeriod getMaxBudgetPeriodByBudgetId(Integer budgetId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<BudgetPeriod> query = builder.createQuery(BudgetPeriod.class);
			Root<BudgetPeriod> rootBudgetPeriod = query.from(BudgetPeriod.class);
			Predicate predicateBudgetId = builder.equal(rootBudgetPeriod.get("budget").get("budgetId"), budgetId);
			query.where(builder.and(predicateBudgetId));
			query.orderBy(builder.desc(rootBudgetPeriod.get("budgetPeriod")));
			return session.createQuery(query).setMaxResults(1).uniqueResult();
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	@Override
	public CostElement fetchCostElementsById(String costElement) {
		return hibernateTemplate.get(CostElement.class, costElement);
	}

	@Override
	public BudgetPeriod saveBudgetPeriod(BudgetPeriod budgetPeriod) {
		try {
			hibernateTemplate.save(budgetPeriod);
		} catch (Exception e) {
			logger.info("Exception in saveBudgetPeriod : {}", e.getMessage());
		}
		return budgetPeriod;
	}

	@Override
	public BudgetPeriod getPeriodById(Integer periodId) {
		return hibernateTemplate.get(BudgetPeriod.class, periodId);
	}

	@Override
	public BudgetDetail saveBudgetDetail(BudgetDetail budgetDetail) {
		try {
			hibernateTemplate.saveOrUpdate(budgetDetail);
		} catch (Exception e) {
			logger.info("Exception in saveBudgetDetail : {}", e.getMessage());
		}
		return budgetDetail;
	}

	@Override
	public BudgetPeriod deleteBudgetPeriod(BudgetPeriod budgetPeriod) {
		hibernateTemplate.delete(budgetPeriod);
		return budgetPeriod;
	}

	@Override
	public BudgetDetail deleteBudgetDetail(BudgetDetail budgetDetail) {
		hibernateTemplate.delete(budgetDetail);
		return budgetDetail;
	}

	@Override
	public List<BudgetDetail> deleteAllBudgetDetail(List<BudgetDetail> budgetDetails) {
		hibernateTemplate.deleteAll(budgetDetails);
		return budgetDetails;
	}

	@Override
	public void deleteBudgetPersonDetail(Integer budgetPersonDetailId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<BudgetPersonalDetails> delete = builder.createCriteriaDelete(BudgetPersonalDetails.class);
		Root<BudgetPersonalDetails> rootBudgetPersonDetails = delete.from(BudgetPersonalDetails.class);
		delete.where(builder.equal(rootBudgetPersonDetails.get("budgetPersonDetailId"), budgetPersonDetailId));
		session.createQuery(delete).executeUpdate();
	}

	@Override
	public List<TbnPerson> fetchAllTbnPerson() {
		return hibernateTemplate.loadAll(TbnPerson.class);
	}

	@Override
	public BudgetHeader saveBudgetHeader(BudgetHeader budgetHeader) {
		try {
			hibernateTemplate.saveOrUpdate(budgetHeader);
		} catch (Exception e) {
			logger.info("Exception in saveBudgetHeader : {}", e.getMessage());
		}
		return budgetHeader;
	}

	@Override
	public BudgetDetailCalcAmount deleteBudgetDetailCalcAmount(BudgetDetailCalcAmount budgetDetailCalcAmount) {
		hibernateTemplate.delete(budgetDetailCalcAmount);
		return budgetDetailCalcAmount;
	}

	@Override
	public List<BudgetStatus> fetchAllBudgetStatus() {
		return hibernateTemplate.loadAll(BudgetStatus.class);
	}

	@Override
	public BudgetStatus getBudgetStatusById(String budgetStatusCode) {
		return hibernateTemplate.get(BudgetStatus.class, budgetStatusCode);
	}

	@Override
	public List<BudgetPerson> getBudgetPersons(Integer budgetId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetPerson> query = builder.createQuery(BudgetPerson.class);
		Root<BudgetPerson> rootBudgetPerson = query.from(BudgetPerson.class);
		Predicate predicateBudgetId = builder.equal(rootBudgetPerson.get("budgetId"), budgetId);
		query.where(builder.and(predicateBudgetId));
		return session.createQuery(query).list();
	}

	@Override
	public BudgetPerson saveOrUpdateProposalBudgetPerson(BudgetPerson budgetPerson) {
		try {
			hibernateTemplate.saveOrUpdate(budgetPerson);
		} catch (Exception e) {
			logger.info("Exception in saveOrUpdateProposalBudgetPerson : {}", e.getMessage());
		}
		return budgetPerson;
	}

	@Override
	public void deleteBudgetPerson(Integer budgetPersonId) {
		hibernateTemplate.delete(hibernateTemplate.get(BudgetPerson.class, budgetPersonId));
	}

	@Override
	public List<AppointmentType> loadAllAppointmentTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AppointmentType> query = builder.createQuery(AppointmentType.class);
		Root<AppointmentType> rootAgreementSponsorType = query.from(AppointmentType.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<JobCode> loadAllJobCodes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<JobCode> query = builder.createQuery(JobCode.class);
		Root<JobCode> rootAgreementSponsorType = query.from(JobCode.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("jobTitle")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<RateType> fetchRateTypeByParams(String rateTypeCode, List<String> rateClassCodes) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<RateType> query = builder.createQuery(RateType.class);
		Root<RateType> rootRateType = query.from(RateType.class);
		Predicate predicateRateClassCode = rootRateType.get(RATECLASSCODE).in(rateClassCodes);
		Predicate predicateRateTypeCode = builder.equal(rootRateType.get(RATETYPECODE), rateTypeCode);
		query.where(builder.and(predicateRateClassCode, predicateRateTypeCode));
		query.orderBy(builder.asc(rootRateType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).list();
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public List<String> fetchRateClassCodesByType(String rateClassType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT rc.code FROM RateClass rc WHERE rc.rateClassTypeCode = :rateClassType";
		Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("rateClassType", rateClassType);
		return query.getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public String fetchBudgetCategoryName(String budgetCategoryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT bc.description FROM BudgetCategory bc WHERE bc.code = :code";
		Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("code", budgetCategoryId);
		return query.getSingleResult();
	}

	@Override
	public List<BudgetHeader> fetchBudgetsByProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetHeader> query = builder.createQuery(BudgetHeader.class);
		Root<BudgetHeader> rootBudgetPerson = query.from(BudgetHeader.class);
		Predicate predicateProposalId = builder.equal(rootBudgetPerson.get("proposalId"), proposalId);
		query.where(builder.and(predicateProposalId));
		return session.createQuery(query).list();
	}

	@SuppressWarnings("unchecked")
	@Override
	public Integer maxBudgetVersionNumberByProposalId(Integer proposalId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT MAX(versionNumber) FROM BudgetHeader WHERE proposalId=:proposalId";
			Query<Integer> query = session.createQuery(hqlQuery);
			query.setParameter(PROPOSALID, proposalId);
			Integer versionNumber = query.uniqueResult();
			return versionNumber == null ? 0 : versionNumber;
		} catch (Exception e) {
			return 0;
		}
	}

	@Override
	public List<BudgetDetail> fetchBudgetDetailBasedOnLineItemNumber(Integer lineItemNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetDetail> query = builder.createQuery(BudgetDetail.class);
		Root<BudgetDetail> rootBudgetDetail = query.from(BudgetDetail.class);
		query.where(builder.equal(rootBudgetDetail.get("lineItemNumber"), lineItemNumber));
		return session.createQuery(query).getResultList();
	}

	@Override
	public BudgetPeriod fetchBudgetPeriodBasedOnPeriodId(Integer budgetPeriodId) {
		return hibernateTemplate.get(BudgetPeriod.class, budgetPeriodId);
	}

	@Override
	public BudgetDetail fetchBudgetDetailBasedOnBudgetDetailId(Integer budgetDetailId) {
		return hibernateTemplate.get(BudgetDetail.class, budgetDetailId);
	}

	@Override
	public BudgetHeader deleteBudgetHeader(BudgetHeader budgetHeader) {
		hibernateTemplate.delete(budgetHeader);
		return budgetHeader;
	}

	@SuppressWarnings("unchecked")
	@Override
	public BudgetHeader getMaxBudgetVersionOfBudget(Integer proposalId) {
		try {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String maxHQLQuery = "SELECT MAX(versionNumber) FROM BudgetHeader WHERE proposalId=:proposalId";
		Query<Integer> countQuery = session.createQuery(maxHQLQuery);
		countQuery.setParameter(PROPOSALID, proposalId);
		Integer maxVersionNumber = countQuery.uniqueResult();
		String hqlQuery = "FROM BudgetHeader WHERE proposalId=:proposalId and versionNumber=:versionNumber";
		Query<BudgetHeader> query = session.createQuery(hqlQuery);
		query.setParameter(PROPOSALID, proposalId);
		query.setParameter("versionNumber", maxVersionNumber);
		return query.uniqueResult();
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public Integer maxBudgetLineItemNumberByBudgetHeader(Integer budgetId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		BudgetHeader budgetHeader = fetchBudgetByBudgetId(budgetId);
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		List<Integer> budgetPeriodIds = new ArrayList<>();
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			budgetPeriodIds.add(budgetPeriod.getBudgetPeriodId());
		}
		if (!budgetPeriodIds.isEmpty()) {
			String hqlQuery = "SELECT MAX(lineItemNumber) FROM BudgetDetail WHERE period.budgetPeriodId in (:budgetPeriodId)";
			Query<Integer> query = session.createQuery(hqlQuery);
			query.setParameterList("budgetPeriodId", budgetPeriodIds);
			return query.uniqueResult();
		} else {
			return 0;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public BudgetHeader fetchActiveBudgetByProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM BudgetHeader WHERE proposalId=:proposalId AND (isFinalBudget = 'Y' or isApprovedBudget = 'Y') order by versionNumber desc";
		Query<BudgetHeader> query = session.createQuery(hqlQuery);
		query.setParameter(PROPOSALID, proposalId);
		List<BudgetHeader> budgetHeaders = query.getResultList();
		if(budgetHeaders.isEmpty()) {
			String maxVersionBudget = "FROM BudgetHeader WHERE proposalId=:proposalId order by versionNumber desc";
			Query<BudgetHeader> queryVersion = session.createQuery(maxVersionBudget);
			queryVersion.setParameter(PROPOSALID, proposalId);
			List<BudgetHeader> headers = queryVersion.getResultList();
			return headers.get(0);
		}
		return budgetHeaders.get(0);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Boolean checkBudgetPersonAddedInBudget(Integer budgetPersonId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT COUNT(*) FROM BudgetPersonalDetails WHERE budgetPersonId=:budgetPersonId";
		Query<Long> query = session.createQuery(hqlQuery);
		query.setParameter(BUDGETPERSONID, budgetPersonId);
		Long count = query.getSingleResult();
		if (count > 0) {
			return true;
		}
		return false;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> fetchRateClassCodesNotInParams(Set<String> rateClasses) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT code FROM RateClass WHERE description not in (:rateClasses)";
		Query<String> query = session.createQuery(hqlQuery);
		query.setParameterList("rateClasses", rateClasses);
		return query.getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> fetchRateTypeCodesByRateClassCode(String rateClassCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT rateTypeCode FROM RateType WHERE rateClassCode=:rateClassCode";
		Query<String> query = session.createQuery(hqlQuery);
		query.setParameter(RATECLASSCODE, rateClassCode);
		return query.getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public String fetchCostElementName(String costElementId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT bc.description FROM CostElement bc WHERE bc.costElement = :code";
		Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("code", costElementId);
		return query.getSingleResult();
	}

	@Override
	public void saveOrUpdateBudgetCalAmount(BudgetDetailCalcAmount budgetDetailCalcAmount) {
		try {
			hibernateTemplate.saveOrUpdate(budgetDetailCalcAmount);
		} catch (Exception e) {
			logger.info("Exception in saveOrUpdateBudgetCalAmount : {}", e.getMessage());
		}
	}

	@Override
	public List<BudgetPersonalDetails> getBudgetPersonDetailsList(Integer budgetPersonId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetPersonalDetails> query = builder.createQuery(BudgetPersonalDetails.class);
		Root<BudgetPersonalDetails> rootBudgetPersonalDetails = query.from(BudgetPersonalDetails.class);
		query.where(builder.equal(rootBudgetPersonalDetails.get(BUDGETPERSONID), budgetPersonId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public BudgetCategory fetchBudgetCategoryBasedOnCode(String budgetCategoryCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<BudgetCategory> query = builder.createQuery(BudgetCategory.class);
			Root<BudgetCategory> rootBudgetCategory = query.from(BudgetCategory.class);
			query.where(builder.equal(rootBudgetCategory.get("code"), budgetCategoryCode));
			return session.createQuery(query).uniqueResult();
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	@Override
	public BudgetPerson getBudgetPersonByPersonId(Integer budgetPersonId) {
		return hibernateTemplate.get(BudgetPerson.class, budgetPersonId);
	}

	@Override
	public List<AwardBudgetPeriod> getAwardBudgetPeriodsByBudgetId(Integer awardbudgetId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetPeriod> query = builder.createQuery(AwardBudgetPeriod.class);
		Root<AwardBudgetPeriod> root = query.from(AwardBudgetPeriod.class);
		query.where(builder.equal(root.get("budgetId"), awardbudgetId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardBudgetDetail> fetchAwardBudgetDetailByPeriodId(Integer budgetPeriodId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetDetail> query = builder.createQuery(AwardBudgetDetail.class);
		Root<AwardBudgetDetail> root = query.from(AwardBudgetDetail.class);
		query.where(builder.equal(root.get("budgetPeriodId"), budgetPeriodId));
		return session.createQuery(query).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public BudgetHeader fetchFinalBudget(Integer proposalId) {
		BudgetHeader budgetHeader = new BudgetHeader();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "FROM BudgetHeader WHERE proposalId=:proposalId and isFinalBudget=:isFinalBudget";
			org.hibernate.query.Query<BudgetHeader> query = session.createQuery(hqlQuery);
			query.setParameter("proposalId", proposalId);
			query.setParameter("isFinalBudget", true);
			budgetHeader = query.getResultList().get(0);
			return budgetHeader;
		} catch (Exception e) {
			return budgetHeader;
		}
	}

	@Override
	public List<CostElement> fetchCostElementNotInBudgetCategoryCode(String categoryCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CostElement> query = builder.createQuery(CostElement.class);
		Root<CostElement> rootCostElement = query.from(CostElement.class);
		if (categoryCode != null) {
			Predicate predicateCategory = builder.equal(rootCostElement.get("budgetCategoryCode"), categoryCode);
			query.where(builder.and(predicateCategory));
		}
		query.orderBy(builder.asc(rootCostElement.get(Constants.DESCRIPTION)));
		return session.createQuery(query).list();
	}

	@Override
	public List<CostSharingType> getCostSharingType() {
		return hibernateTemplate.loadAll(CostSharingType.class);
	}

	@Override
	public BudgetPersonalDetails getBudgetPersonalDetailsById(Integer budgetPersonDetailId) {
		return hibernateTemplate.get(BudgetPersonalDetails.class, budgetPersonDetailId);
	}

	@Override
	public boolean checkBudgetPersonInBudget(Integer budgetId, String tbnId, String jobCodeType, String personType, String personId, Integer rolodexId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetPerson> query = builder.createQuery(BudgetPerson.class);
		Root<BudgetPerson> rootBudgetPerson = query.from(BudgetPerson.class);
		Predicate predicateBudgetId = builder.equal(rootBudgetPerson.get("budgetId"), budgetId);
		Predicate predicateJobCodeType = builder.equal(rootBudgetPerson.get("jobCodeType"), jobCodeType);
		Predicate predicateEmployeeType = null;
		if (personId != null && (personType.equals(Constants.EMPLOYEE_PERSON_TYPE) || personType.equals(Constants.PROPOSAL_PERSON_TYPE))) {
			predicateEmployeeType = builder.equal(rootBudgetPerson.get("personId"), personId);
		} else if(rolodexId != null && (personType.equals(Constants.NON_EMPLOYEE_TYPE) || personType.equals(Constants.PROPOSAL_PERSON_TYPE))) {
			predicateEmployeeType = builder.equal(rootBudgetPerson.get("rolodexId"), rolodexId);
		} else {
			predicateEmployeeType = builder.equal(rootBudgetPerson.get("tbnId"), tbnId);
		}
		query.where(builder.and(predicateBudgetId, predicateJobCodeType, predicateEmployeeType));
		List<BudgetPerson> budgetPersons = session.createQuery(query).getResultList();
		return budgetPersons.isEmpty();
	}

	@Override
	public BudgetPerson getBugetTbnPersonByTbnId(Integer budgetId, String personType, String tbnId, String appointmentTypeCode, String jobCodeType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetPerson> query = builder.createQuery(BudgetPerson.class);
		Root<BudgetPerson> rootBudgetPerson = query.from(BudgetPerson.class);
		Predicate predicateBudgetId = builder.equal(rootBudgetPerson.get("budgetId"), budgetId);
		Predicate predicateJobCodeType = builder.equal(rootBudgetPerson.get("jobCodeType"), jobCodeType);
		Predicate predicatePersonType = builder.equal(rootBudgetPerson.get("personType"), personType);
		Predicate predicateAppointmentType = builder.equal(rootBudgetPerson.get("appointmentTypeCode"), appointmentTypeCode);
		Predicate predicateTbnId = builder.equal(rootBudgetPerson.get("tbnId"), tbnId);
		query.where(builder.and(predicateBudgetId, predicateJobCodeType, predicatePersonType, predicateAppointmentType, predicateTbnId));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public BudgetPerson getBugetPersonByPersonId(Integer budgetId, String personType, String personId, String appointmentTypeCode, String jobTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetPerson> query = builder.createQuery(BudgetPerson.class);
		Root<BudgetPerson> rootBudgetPerson = query.from(BudgetPerson.class);
		Predicate predicateBudgetId = builder.equal(rootBudgetPerson.get("budgetId"), budgetId);
		Predicate predicateJobCodeType = builder.equal(rootBudgetPerson.get("jobCodeType"), jobTypeCode);
		Predicate predicatePersonType = builder.equal(rootBudgetPerson.get("personType"), personType);
		Predicate predicateAppointmentType = builder.equal(rootBudgetPerson.get("appointmentTypeCode"), appointmentTypeCode);
		Predicate predicatePersonId = builder.equal(rootBudgetPerson.get("personId"), personId);
		query.where(builder.and(predicateBudgetId, predicateJobCodeType, predicatePersonType, predicateAppointmentType, predicatePersonId));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public BudgetPerson getBugetRolodexPersonByRolodexId(Integer budgetId, String personType, Integer rolodexId, String appointmentTypeCode, String jobCodeType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetPerson> query = builder.createQuery(BudgetPerson.class);
		Root<BudgetPerson> rootBudgetPerson = query.from(BudgetPerson.class);
		Predicate predicateBudgetId = builder.equal(rootBudgetPerson.get("budgetId"), budgetId);
		Predicate predicateJobCodeType = builder.equal(rootBudgetPerson.get("jobCodeType"), jobCodeType);
		Predicate predicatePersonType = builder.equal(rootBudgetPerson.get("personType"), personType);
		Predicate predicateAppointmentType = builder.equal(rootBudgetPerson.get("appointmentTypeCode"), appointmentTypeCode);
		Predicate predicateRolodexId = builder.equal(rootBudgetPerson.get("rolodexId"), rolodexId);
		query.where(builder.and(predicateBudgetId, predicateJobCodeType, predicatePersonType, predicateAppointmentType, predicateRolodexId));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public List<BudgetTemplateType> getBudgetTemplateTypesByModuleCode(Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetTemplateType> query = builder.createQuery(BudgetTemplateType.class);
		Root<BudgetTemplateType> templateType = query.from(BudgetTemplateType.class);
		Predicate predicateModuleCode = builder.equal(templateType.get("moduleCode"), moduleCode);
		Predicate predicateIsActive = builder.equal(templateType.get("isActive"), true);
		query.where(builder.and(predicateModuleCode, predicateIsActive));
		return 	session.createQuery(query).getResultList();
	}

	@Override
	public List<BudgetTemplate> fetchBudgetTemplatesByTemplateType(Integer budgetTemplateTypeId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetTemplate> query = builder.createQuery(BudgetTemplate.class);
		Root<BudgetTemplate> budgetTemplate = query.from(BudgetTemplate.class);
		Predicate predicateBudgetTemplate = builder.equal(budgetTemplate.get("budgetTemplateTypeId"), budgetTemplateTypeId);
		query.where(builder.and(predicateBudgetTemplate));
		return session.createQuery(query).list();
	}

	@Override
	public void updateProposalBudgetStatus(Integer proposalId, String budgetStatusCode, String updateUser) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<BudgetHeader> criteriaUpdate = cb.createCriteriaUpdate(BudgetHeader.class);
		Root<BudgetHeader> rootBudgetHeader = criteriaUpdate.from(BudgetHeader.class);
		Predicate predicateProposalId = cb.equal(rootBudgetHeader.get("proposalId"), proposalId);
		Predicate isLatestVersion = cb.equal(rootBudgetHeader.get("isLatestVersion"), Boolean.TRUE);
		criteriaUpdate.set("budgetStatusCode", budgetStatusCode);
		if (budgetStatusCode.equals(Constants.BUDGET_STATUS_COMPLETED)) {
			criteriaUpdate.set("isFinalBudget", Boolean.TRUE);
		}
		criteriaUpdate.set("updateUser", updateUser);
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.where(predicateProposalId, isLatestVersion);		 		
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public BudgetTemplateType fetchBudgetTemplateTypeById(Integer budgetTemplateTypeId) {
		return hibernateTemplate.get(BudgetTemplateType.class, budgetTemplateTypeId);
	}

	@Override
	public BudgetHeader fetchLinkedBudgetsByProposalNumber(String proposalNumber) {
		BudgetHeader budgetHeader = getIPLinkedApprovedBudgetHeader(proposalNumber);
		if (budgetHeader == null) {
			return getIPLinkedFinalBudgetHeader(proposalNumber);
		}
		return budgetHeader;
	}

	private BudgetHeader getIPLinkedApprovedBudgetHeader(String proposalNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			StringBuilder hqlQuery = new StringBuilder("select T1 from BudgetHeader T1 inner join InstituteProposalAdminDetail T3 on T3.devProposalId = T1.proposalId").
					append(" and T3.proposalAdminDetailId in (:adminDetailId)").
					append(" inner join InstituteProposal T4 on T4.proposalId = T3.instProposalId where T4.proposalNumber =: proposalNumber and T4.proposalSequenceStatus = 'ACTIVE' and  T1.isApprovedBudget =:isApprovedBudget");
			javax.persistence.Query query = session.createQuery(hqlQuery.toString());
			query.setParameter("proposalNumber", proposalNumber);
			query.setParameter("adminDetailId", getAdminDetailIdForApprovedBudgetHeader(proposalNumber));
			query.setParameter("isApprovedBudget", true);
			return (BudgetHeader) query.getSingleResult();
		} catch (Exception e) {
			logger.error("error occured in getIPLinkedBudgetHeader : {}", e.getMessage());
			return null;
		}
	}

	private BudgetHeader getIPLinkedFinalBudgetHeader(String proposalNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			StringBuilder hqlQuery = new StringBuilder("select T1 from BudgetHeader T1 inner join InstituteProposalAdminDetail T3 on T3.devProposalId = T1.proposalId").
					append(" and T3.proposalAdminDetailId in (:adminDetailId)").
					append(" inner join InstituteProposal T4 on T4.proposalId = T3.instProposalId where T4.proposalNumber =: proposalNumber and T4.proposalSequenceStatus = 'ACTIVE' and  T1.isFinalBudget =:isFinalBudget");
			javax.persistence.Query query = session.createQuery(hqlQuery.toString());
			query.setParameter("proposalNumber", proposalNumber);
			query.setParameter("adminDetailId", getAdminDetailIdForFinalBudgetHeader(proposalNumber));
			query.setParameter("isFinalBudget", true);
			return (BudgetHeader) query.getSingleResult();
		} catch (Exception e) {
			logger.error("error occured in getIPLinkedBudgetHeader : {}", e.getMessage());
			return null;
		}
	}

	@SuppressWarnings("rawtypes")
	private Integer getAdminDetailIdForApprovedBudgetHeader(String proposalNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			StringBuilder sql = new StringBuilder("SELECT MAX(S1.PROPOSAL_ADMIN_DETAIL_ID) FROM PROPOSAL_ADMIN_DETAILS S1 INNER JOIN BUDGET_HEADER S3 ON S3.PROPOSAL_ID =S1.DEV_PROPOSAL_ID AND S3.IS_APPROVED_BUDGET ='Y'").
			append(" INNER JOIN PROPOSAL S4 ON S4.PROPOSAL_ID = S1.INST_PROPOSAL_ID WHERE S4.PROPOSAL_NUMBER =:proposalNumber AND S4.PROPOSAL_SEQUENCE_STATUS = 'ACTIVE'");
			Query query = session.createSQLQuery(sql.toString());
			query.setParameter("proposalNumber", proposalNumber);
			Object result = query.getSingleResult();
			return (result != null) ? Integer.parseInt(result.toString()) : null;
		} catch (Exception e) {
			logger.error("error occured in getAdminDetailIdForBudgetHeader : {}", e.getMessage());
			return null;
		}
	}

	@SuppressWarnings("rawtypes")
	private Integer getAdminDetailIdForFinalBudgetHeader(String proposalNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			StringBuilder sql = new StringBuilder("SELECT MAX(S1.PROPOSAL_ADMIN_DETAIL_ID) FROM PROPOSAL_ADMIN_DETAILS S1 INNER JOIN BUDGET_HEADER S3 ON S3.PROPOSAL_ID =S1.DEV_PROPOSAL_ID AND S3.IS_FINAL_BUDGET ='Y'").
			append(" INNER JOIN PROPOSAL S4 ON S4.PROPOSAL_ID = S1.INST_PROPOSAL_ID WHERE S4.PROPOSAL_NUMBER =:proposalNumber AND S4.PROPOSAL_SEQUENCE_STATUS = 'ACTIVE'");
			Query query = session.createSQLQuery(sql.toString());
			query.setParameter("proposalNumber", proposalNumber);
			Object result = query.getSingleResult();
			return (result != null) ? Integer.parseInt(result.toString()) : null;
		} catch (Exception e) {
			logger.error("error occured in getAdminDetailIdForBudgetHeader : {}", e.getMessage());
			return null;
		}
	}

	@Override
	public void updateBudgetLatestVersionFlag(Integer proposalId, Integer budgetVersionNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder sql = new StringBuilder("update BudgetHeader set isLatestVersion =:isLatestVersion where proposalId=:proposalId and versionNumber <:versionNumber and isLatestVersion = true ");
		Query<?> query = session.createQuery(sql.toString());
		query.setParameter("isLatestVersion", false);
		query.setParameter("proposalId", proposalId);
		query.setParameter("versionNumber", budgetVersionNumber);
		query.executeUpdate();
	}

	@Override
	public List<FundDisbursementBasisType> getFundDisbursementBasisType() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<FundDisbursementBasisType> query = builder.createQuery(FundDisbursementBasisType.class);
		Root<FundDisbursementBasisType> rootAgreementSponsorType = query.from(FundDisbursementBasisType.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}
}
