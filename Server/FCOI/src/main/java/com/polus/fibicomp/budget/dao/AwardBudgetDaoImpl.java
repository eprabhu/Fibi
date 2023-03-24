package com.polus.fibicomp.budget.dao;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.transform.Transformers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.adminportal.dao.UnitHierarchyDao;
import com.polus.fibicomp.adminportal.pojo.InstituteRate;
import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.vo.AwardBudgetImportInfo;
import com.polus.fibicomp.award.vo.BudgetImportPeriodsInfo;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetailCalcAmount;
import com.polus.fibicomp.budget.pojo.AwardBudgetFundType;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.pojo.AwardBudgetNonPersonDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetPeriod;
import com.polus.fibicomp.budget.pojo.AwardBudgetPerson;
import com.polus.fibicomp.budget.pojo.AwardBudgetPersonalDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetStatus;
import com.polus.fibicomp.budget.pojo.AwardRates;
import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.BudgetType;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.budget.pojo.TbnPerson;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fastintegration.pojo.SapAwardFeed;

@SuppressWarnings("deprecation")
@Transactional
@Service(value = "awardBudgetDao")
public class AwardBudgetDaoImpl implements AwardBudgetDao {

	protected static Logger logger = LogManager.getLogger(AwardBudgetDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private UnitHierarchyDao unitHierarchyDao;

	@SuppressWarnings("unchecked")
	@Override
	public List<InstituteRate> filterInstituteRateByDateRange(Timestamp startDate, Timestamp endDate,
			String activityTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(InstituteRate.class);
		criteria.add(Restrictions.eq("activityTypeCode", activityTypeCode));
		criteria.add(Restrictions.between("startDate", startDate, endDate));
		criteria.addOrder(Order.asc("startDate"));
		criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
		List<InstituteRate> instituteRates = criteria.list();
		return instituteRates;
	}

	@Override
	public List<CostElement> getAllCostElements() {
		return hibernateTemplate.loadAll(CostElement.class);
	}

	@Override
	public RateType getOHRateTypeByParams(String rateClassCode, String rateTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(RateType.class);
		criteria.add(Restrictions.eq("rateClassCode", rateClassCode));
		criteria.add(Restrictions.eq("rateTypeCode", rateTypeCode));
		RateType rateType = (RateType) criteria.uniqueResult();
		return rateType;
	}

	@Override
	public AwardRates fetchApplicableAwardRate(Integer budgetId, Timestamp budgetStartDate, String rateClassCode,
			String rateTypeCode, String activityTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		AwardRates applicableRate = null;
		Criteria criteria = session.createCriteria(AwardRates.class);
		criteria.add(Restrictions.eq("budgetHeaderId", budgetId));
		criteria.add(Restrictions.le("startDate", budgetStartDate));
		criteria.add(Restrictions.eq("rateClassCode", rateClassCode));
		criteria.add(Restrictions.eq("rateTypeCode", rateTypeCode));
		criteria.add(Restrictions.eq("activityTypeCode", activityTypeCode));
		criteria.addOrder(Order.desc("startDate"));
		@SuppressWarnings("unchecked")
		List<AwardRates> proposalrate = criteria.list();
		if (proposalrate != null && !proposalrate.isEmpty()) {
			applicableRate = proposalrate.get(0);
		}
		return applicableRate;
	}

	@Override
	public AwardBudgetHeader fetchBudgetByBudgetId(Integer budgetId) {
		return hibernateTemplate.get(AwardBudgetHeader.class, budgetId);
	}

	@Override
	public InstituteRate fetchInstituteRateByDateLessthanMax(Timestamp startDate, String activityTypeCode,
			String rateClassCode, String rateTypeCode) {
		InstituteRate instituteRate = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(InstituteRate.class);
		criteria.add(Restrictions.eq("activityTypeCode", activityTypeCode));
		criteria.add(Restrictions.eq("rateClassCode", rateClassCode));
		criteria.add(Restrictions.eq("rateTypeCode", rateTypeCode));
		criteria.add(Restrictions.le("startDate", startDate));
		criteria.addOrder(Order.desc("startDate"));
		criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
		@SuppressWarnings("unchecked")
		List<InstituteRate> instituteRates = criteria.list();
		if (instituteRates != null && !instituteRates.isEmpty()) {
			instituteRate = instituteRates.get(0);
		}
		return instituteRate;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<CostElement> fetchCostElementsByIds(List<String> costElements) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(CostElement.class);
		criteria.add(Restrictions.in("costElement", costElements));
		return criteria.list();
	}

	@Override
	public AwardBudgetPeriod getMaxBudgetPeriodByBudgetId(Integer budgetId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(AwardBudgetPeriod.class);
		criteria.add(Restrictions.eq("budgetId", budgetId));
		criteria.addOrder(Order.desc("budgetPeriod"));
		criteria.setMaxResults(1);
		AwardBudgetPeriod budgetPeriod = (AwardBudgetPeriod) criteria.uniqueResult();
		return budgetPeriod;
	}

	@Override
	public CostElement fetchCostElementsById(String costElement) {
		return hibernateTemplate.get(CostElement.class, costElement);
	}

	@Override
	public AwardBudgetPeriod saveBudgetPeriod(AwardBudgetPeriod budgetPeriod) {
		try {
			hibernateTemplate.save(budgetPeriod);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return budgetPeriod;
	}

	@Override
	public List<BudgetCategory> fetchAllBudgetCategory() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(BudgetCategory.class);
		ProjectionList projList = Projections.projectionList();
		projList.add(Projections.property("code"), "code");
		projList.add(Projections.property("budgetCategoryTypeCode"), "budgetCategoryTypeCode");
		projList.add(Projections.property(Constants.DESCRIPTION), Constants.DESCRIPTION);
		criteria.setProjection(projList).setResultTransformer(Transformers.aliasToBean(BudgetCategory.class));
		criteria.addOrder(Order.asc(Constants.DESCRIPTION));
		@SuppressWarnings("unchecked")
		List<BudgetCategory> budgetCategories = criteria.list();
		return budgetCategories;
	}

	@Override
	public List<CostElement> fetchCostElementByBudgetCategory(String budgetCategoryCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(CostElement.class);
		ProjectionList projList = Projections.projectionList();
		projList.add(Projections.property("costElement"), "costElement");
		projList.add(Projections.property(Constants.DESCRIPTION), Constants.DESCRIPTION);
		criteria.setProjection(projList).setResultTransformer(Transformers.aliasToBean(CostElement.class));
		criteria.add(Restrictions.eq("budgetCategoryCode", budgetCategoryCode));
		criteria.addOrder(Order.asc(Constants.DESCRIPTION));
		@SuppressWarnings("unchecked")
		List<CostElement> costElements = criteria.list();
		return costElements;
	}

	@Override
	public AwardBudgetPeriod getPeriodById(Integer periodId) {
		return hibernateTemplate.get(AwardBudgetPeriod.class, periodId);
	}

	@Override
	public AwardBudgetDetail saveBudgetDetail(AwardBudgetDetail budgetDetail) {
		try {
			hibernateTemplate.save(budgetDetail);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return budgetDetail;
	}

	@Override
	public AwardBudgetPeriod deleteBudgetPeriod(AwardBudgetPeriod budgetPeriod) {
		hibernateTemplate.delete(budgetPeriod);
		return budgetPeriod;
	}

	@Override
	public AwardBudgetDetail deleteBudgetDetail(AwardBudgetDetail budgetDetail) {
		try {
			hibernateTemplate.delete(budgetDetail);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return budgetDetail;
	}

	@Override
	public List<AwardBudgetDetail> deleteAllBudgetDetail(List<AwardBudgetDetail> budgetDetail) {
		hibernateTemplate.deleteAll(budgetDetail);
		return budgetDetail;
	}

	@Override
	public void deleteBudgetPersonDetail(Integer budgetPersonDetailId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AwardBudgetPersonalDetail> delete = builder.createCriteriaDelete(AwardBudgetPersonalDetail.class);
		Root<AwardBudgetPersonalDetail> root = delete.from(AwardBudgetPersonalDetail.class);
		delete.where(builder.equal(root.get("budgetPersonDetailId"), budgetPersonDetailId));
		session.createQuery(delete).executeUpdate();
	}

	@Override
	public List<TbnPerson> fetchAllTbnPerson() {
		return hibernateTemplate.loadAll(TbnPerson.class);
	}

	@Override
	public AwardBudgetHeader saveBudgetHeader(AwardBudgetHeader budgetHeader) {
		try {
			hibernateTemplate.saveOrUpdate(budgetHeader);
		} catch (Exception e) {
			throw new ApplicationException("Error in saveBudgetHeader", e, Constants.JAVA_ERROR);
		}
		return budgetHeader;
	}

	@Override
	public AwardBudgetDetailCalcAmount deleteBudgetDetailCalcAmount(
			AwardBudgetDetailCalcAmount budgetDetailCalcAmount) {
		hibernateTemplate.delete(budgetDetailCalcAmount);
		return budgetDetailCalcAmount;
	}

	@SuppressWarnings("unchecked")
	@Override
	public AwardBudgetHeader getAwardBudgetHeaderByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String maxHQLQuery = "SELECT MAX(versionNumber) FROM AwardBudgetHeader WHERE awardId=:awardId";
		org.hibernate.query.Query<Integer> countQuery = session.createQuery(maxHQLQuery);
		countQuery.setParameter("awardId", awardId);
		Integer maxVersionNumber = countQuery.uniqueResult();
		return getBudgetHeaderByVersionNumber(awardId, maxVersionNumber);
	}

	@Override
	public BudgetType getBudgetTypeById(String budgetTypeCode) {
		return hibernateTemplate.get(BudgetType.class, budgetTypeCode);
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

	@SuppressWarnings({ "unchecked" })
	@Override
	public List<AwardBudgetImportInfo> getDevProposalBudgets(Integer awardId) {
		List<AwardBudgetImportInfo> awardBudgetImportInfos = new ArrayList<AwardBudgetImportInfo>();
		List<Object[]> budgetImports = new ArrayList<Object[]>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder sql = new StringBuilder("SELECT DISTINCT T1.AWARD_NUMBER, T2.PROPOSAL_NUMBER, T3.PROPOSAL_ID, T3.TITLE, T4.VERSION_NUMBER, T4.BUDGET_HEADER_ID FROM AWARD T1 LEFT OUTER JOIN AWARD_FUNDING_PROPOSALS T5 ON T1.AWARD_ID = T5.AWARD_ID "). 
				append("LEFT OUTER JOIN PROPOSAL T2 ON T5.PROPOSAL_ID = T2.PROPOSAL_ID LEFT OUTER JOIN EPS_PROPOSAL T3 ON T3.IP_NUMBER = T2.PROPOSAL_NUMBER LEFT OUTER JOIN BUDGET_HEADER T4 ON T3.PROPOSAL_ID = T4.PROPOSAL_ID ").
				append("INNER JOIN PROPOSAL_ADMIN_DETAILS T6 ON T6.DEV_PROPOSAL_ID = T4.PROPOSAL_ID AND T6.PROPOSAL_ADMIN_DETAIL_ID = (SELECT MAX(S1.PROPOSAL_ADMIN_DETAIL_ID) FROM PROPOSAL_ADMIN_DETAILS S1 "). 
				append("INNER JOIN BUDGET_HEADER S3 ON S3.PROPOSAL_ID =S1.DEV_PROPOSAL_ID AND S3.IS_APPROVED_BUDGET ='Y' INNER JOIN AWARD_FUNDING_PROPOSALS S4 ON S4.PROPOSAL_ID = S1.INST_PROPOSAL_ID WHERE S4.AWARD_ID =:awardId ").
				append(") WHERE T1.AWARD_ID =:awardId AND T4.IS_APPROVED_BUDGET ='Y'");
		javax.persistence.Query approvedBudgetquery = session.createSQLQuery(sql.toString());	
		approvedBudgetquery.setParameter("awardId", awardId);
		budgetImports = approvedBudgetquery.getResultList();
		if (budgetImports.isEmpty()) {
			sql = new StringBuilder("SELECT DISTINCT T1.AWARD_NUMBER, T2.PROPOSAL_NUMBER, T3.PROPOSAL_ID, T3.TITLE, T4.VERSION_NUMBER, T4.BUDGET_HEADER_ID FROM AWARD T1 LEFT OUTER JOIN AWARD_FUNDING_PROPOSALS T5 ON T1.AWARD_ID = T5.AWARD_ID "). 
			append("LEFT OUTER JOIN PROPOSAL T2 ON T5.PROPOSAL_ID = T2.PROPOSAL_ID LEFT OUTER JOIN EPS_PROPOSAL T3 ON T3.IP_NUMBER = T2.PROPOSAL_NUMBER LEFT OUTER JOIN BUDGET_HEADER T4 ON T3.PROPOSAL_ID = T4.PROPOSAL_ID ").
			append("INNER JOIN PROPOSAL_ADMIN_DETAILS T6 ON T6.DEV_PROPOSAL_ID = T4.PROPOSAL_ID AND T6.PROPOSAL_ADMIN_DETAIL_ID = (SELECT MAX(S1.PROPOSAL_ADMIN_DETAIL_ID) FROM PROPOSAL_ADMIN_DETAILS S1 "). 
			append("INNER JOIN BUDGET_HEADER S3 ON S3.PROPOSAL_ID =S1.DEV_PROPOSAL_ID AND S3.IS_FINAL_BUDGET ='Y' INNER JOIN AWARD_FUNDING_PROPOSALS S4 ON S4.PROPOSAL_ID = S1.INST_PROPOSAL_ID WHERE S4.AWARD_ID =:awardId ").
			append(") WHERE T1.AWARD_ID =:awardId AND T4.IS_FINAL_BUDGET ='Y'");
			javax.persistence.Query finalBudgetquery = session.createSQLQuery(sql.toString());
			finalBudgetquery.setParameter("awardId", awardId);
			budgetImports = finalBudgetquery.getResultList();
		}
		for (Object[] budgetImport : budgetImports) {
			AwardBudgetImportInfo awardBudgetImportInfo = new AwardBudgetImportInfo();
			awardBudgetImportInfo.setAwardNumber(budgetImport[0].toString());
			awardBudgetImportInfo.setIpNumber(budgetImport[1].toString());
			awardBudgetImportInfo.setPdNumber(Integer.parseInt(budgetImport[2].toString()));
			awardBudgetImportInfo.setPdTitle(budgetImport[3].toString());
			awardBudgetImportInfo.setPdBudgetVersion(Integer.parseInt(budgetImport[4].toString()));
			awardBudgetImportInfo.setPdBudgetId(Integer.parseInt(budgetImport[5].toString()));
			awardBudgetImportInfos.add(awardBudgetImportInfo);
		}
		return awardBudgetImportInfos;
	}

	@SuppressWarnings({ "unchecked"})
	@Override
	public List<BudgetImportPeriodsInfo> getDevProposalBudgetPeriods(Integer awardbudgetId) {
		List<BudgetImportPeriodsInfo> budgetImportPeriods = new ArrayList<BudgetImportPeriodsInfo>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		javax.persistence.Query  query = session.createSQLQuery(
				"SELECT BUDGET_PERIOD_ID, BUDGET_HEADER_ID, BUDGET_PERIOD, START_DATE, END_DATE, TOTAL_COST FROM BUDGET_PERIOD WHERE BUDGET_HEADER_ID = :awardbudgetId");
		query.setParameter("awardbudgetId", awardbudgetId);
		List<Object[]> budgetPeriodList = query.getResultList();
		for (Object[] budgetPeriod : budgetPeriodList) {
			BudgetImportPeriodsInfo budgetPeriodInfo = new BudgetImportPeriodsInfo();
			budgetPeriodInfo.setBudgetPeriodId(Integer.parseInt(budgetPeriod[0].toString()));
			budgetPeriodInfo.setBudgetId(Integer.parseInt(budgetPeriod[1].toString()));
			budgetPeriodInfo.setPeriodNumber(Integer.parseInt(budgetPeriod[2].toString()));
			budgetPeriodInfo.setPeriodStartDate((Date) budgetPeriod[3]);
			budgetPeriodInfo.setPeriodEndDate((Date) budgetPeriod[4]);
			budgetPeriodInfo.setTotalCost(budgetPeriod[5].toString());
			budgetImportPeriods.add(budgetPeriodInfo);
		}
		return budgetImportPeriods;
	}

	@Override
	public List<AwardBudgetHeader> getAwardBudgetVersionsByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetHeader> query = builder.createQuery(AwardBudgetHeader.class);
		Root<AwardBudgetHeader> root = query.from(AwardBudgetHeader.class);
		query.where(builder.equal(root.get("awardId"), awardId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardBudgetStatus getAwardBudgetStatusById(String awardBudgetStatusCode) {
		return hibernateTemplate.get(AwardBudgetStatus.class, awardBudgetStatusCode);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Integer maxAwardBudgetVersionNumberByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT MAX(versionNumber) FROM AwardBudgetHeader WHERE awardId=:awardId";
		org.hibernate.query.Query<Integer> query = session.createQuery(hqlQuery);
		query.setParameter("awardId", awardId);
		return query.uniqueResult();
	}

	@Override
	public AwardBudgetPerson saveOrUpdateAwardBudgetPerson(AwardBudgetPerson awardBudgetPerson) {
		try {
			hibernateTemplate.saveOrUpdate(awardBudgetPerson);
		} catch (Exception e) {
			e.printStackTrace();
			throw new ApplicationException("Error in saveOrUpdateAwardBudgetPerson", e, Constants.JAVA_ERROR);
		}
		return awardBudgetPerson;
	}

	@Override
	public List<AwardBudgetPerson> getBudgetPersons(Integer budgetHeaderId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetPerson> query = builder.createQuery(AwardBudgetPerson.class);
		Root<AwardBudgetPerson> root = query.from(AwardBudgetPerson.class);
		Predicate predicate1 = builder.equal(root.get("budgetHeaderId"), budgetHeaderId);
		query.where(builder.and(predicate1));
		return session.createQuery(query).list();
	}

	@Override
	public void deleteAwardBudgetPerson(Integer budgetPersonId) {
		hibernateTemplate.delete(hibernateTemplate.get(AwardBudgetPerson.class, budgetPersonId));
	}

	@Override
	public List<AwardBudgetHeader> fetchAwardBudgetHeaderByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM AwardBudgetHeader WHERE awardId=:awardId";
		Query<AwardBudgetHeader> query = session.createQuery(hqlQuery);
		query.setParameter("awardId", awardId);
		return query.getResultList();
	}

	@Override
	public boolean checkBudgetPersonInBudget(Integer budgetHeaderId, String tbnId, String jobCode, String personId,
			Integer rolodexId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetPerson> query = builder.createQuery(AwardBudgetPerson.class);
		Root<AwardBudgetPerson> root = query.from(AwardBudgetPerson.class);
		Predicate predicate1 = builder.equal(root.get("budgetId"), budgetHeaderId);
		Predicate predicate3 = builder.equal(root.get("jobCodeType"), jobCode);
		Predicate predicate2 = null;
		if (!personId.equals(null)) {
			predicate2 = builder.equal(root.get("personId"), personId);
		} else if (!tbnId.equals(null)) {
			predicate2 = builder.equal(root.get("tbnId"), tbnId);
		} else {
			predicate2 = builder.equal(root.get("rolodexId"), rolodexId);
		}
		query.where(builder.and(predicate1, predicate2, predicate3));
		if (!(session.createQuery(query).list()).isEmpty() && (session.createQuery(query).list()) != null) {
			return false;
		} else {
			return true;
		}
	}

	@Override
	public AwardBudgetPerson getBugetTbnPersonByTbnId(Integer budgetHeaderId, String tbnId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM AwardBudgetPerson bc where bc.budgetId = :budgetId AND bc.tbnId =:tbnId";
		Query<AwardBudgetPerson> query = session.createQuery(hqlQuery);
		query.setParameter("budgetId", budgetHeaderId);
		query.setParameter("tbnId", tbnId);
		AwardBudgetPerson result = query.getSingleResult();
		return result;
	}

	@Override
	public AwardBudgetPerson getBugetPersonByPersonId(Integer budgetHeaderId, String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM AwardBudgetPerson bc where bc.budgetId = :budgetId AND bc.personId =:personId";
		Query<AwardBudgetPerson> query = session.createQuery(hqlQuery);
		query.setParameter("budgetId", budgetHeaderId);
		query.setParameter("personId", personId);
		AwardBudgetPerson result = query.getSingleResult();
		return result;
	}

	@Override
	public AwardBudgetPerson getBugetRolodexPersonByRolodexId(Integer budgetHeaderId, Integer rolodexId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM AwardBudgetPerson bc where bc.budgetId = :budgetId AND bc.rolodexId =:rolodexId";
		Query<AwardBudgetPerson> query = session.createQuery(hqlQuery);
		query.setParameter("budgetId", budgetHeaderId);
		query.setParameter("rolodexId", rolodexId);
		AwardBudgetPerson result = query.getSingleResult();
		return result;
	}

	@Override
	public AwardBudgetHeader saveOrUpdateAwardBudgetOverView(AwardBudgetHeader budgetHeader) {
		hibernateTemplate.saveOrUpdate(budgetHeader);
		return budgetHeader;
	}

	@Override
	public AwardBudgetPeriod saveOrUpdateAwardBudgetPeriod(AwardBudgetPeriod budgetPeriod) {
		hibernateTemplate.saveOrUpdate(budgetPeriod);
		return budgetPeriod;
	}

	@Override
	public AwardBudgetDetail saveOrUpdateAwardBudgetLineItem(AwardBudgetDetail awardBudgetDetail) {
		try {
			hibernateTemplate.saveOrUpdate(awardBudgetDetail);
		} catch (Exception e) {
			e.printStackTrace();
			throw new ApplicationException("Error in saveOrUpdateAwardBudgetLineItem", e, Constants.JAVA_ERROR);
		}
		return awardBudgetDetail;
	}

	@Override
	public AwardBudgetHeader saveAwardBudgetHeader(AwardBudgetHeader awardBudgetHeader) {
		hibernateTemplate.merge(awardBudgetHeader);
		return awardBudgetHeader;
	}

	@Override
	public AwardBudgetPerson getAwardBudgetPersonBypersonId(Integer budgetPersonId) {
		return hibernateTemplate.get(AwardBudgetPerson.class, budgetPersonId);
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

	@Override
	public List<AwardBudgetPersonalDetail> fetchAwardPersonDetailsBydetailedId(Integer budgetDetailId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetPersonalDetail> query = builder.createQuery(AwardBudgetPersonalDetail.class);
		Root<AwardBudgetPersonalDetail> root = query.from(AwardBudgetPersonalDetail.class);
		query.where(builder.equal(root.get("budgetDetailId"), budgetDetailId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void saveOrUpdateBudgetPersonDetails(AwardBudgetPersonalDetail persons) {
		hibernateTemplate.saveOrUpdate(persons);
	}

	@Override
	public void deleteAwardBudgetPersonDetail(AwardBudgetPersonalDetail persons) {
		hibernateTemplate.delete(persons);
	}

	@Override
	public List<AwardBudgetDetail> fetchSysGeneratedCE(Integer budgetPeriodId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetDetail> query = builder.createQuery(AwardBudgetDetail.class);
		Root<AwardBudgetDetail> root = query.from(AwardBudgetDetail.class);
		Predicate predicate1 = builder.equal(root.get("budgetPeriodId"), budgetPeriodId);
		Predicate predicate2 = builder.equal(root.get("isSystemGeneratedCostElement"), true);
		query.where(builder.and(predicate1, predicate2));
		return session.createQuery(query).list();
	}

	@Override
	public void deleteAllAwardBudgetLineItem(List<AwardBudgetDetail> awardBudgetDetail) {
		hibernateTemplate.deleteAll(awardBudgetDetail);
	}

	@Override
	public List<AwardBudgetPersonalDetail> getAwardBudgetPersonalDetailsByDetailId(Integer budgetDetailId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetPersonalDetail> query = builder.createQuery(AwardBudgetPersonalDetail.class);
		Root<AwardBudgetPersonalDetail> rootAwardBudgetPersonalDetail = query.from(AwardBudgetPersonalDetail.class);
		query.where(builder.equal(rootAwardBudgetPersonalDetail.get("budgetDetail").get("budgetDetailId"), budgetDetailId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Boolean checkBudgetPerson(Integer budgetId, String personId, Integer rolodexId, String personType,
			String tbnId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetPerson> query = builder.createQuery(AwardBudgetPerson.class);
		Root<AwardBudgetPerson> root = query.from(AwardBudgetPerson.class);
		Predicate predicate1 = builder.equal(root.get("budgetHeaderId"), budgetId);
		Predicate predicate2 = null;
		if (personType.equals(Constants.EMPLOYEE_PERSON_TYPE)) {
			predicate2 = builder.equal(root.get("personId"), personId);
		} else if (personType.equals(Constants.NON_EMPLOYEE_TYPE)) {
			predicate2 = builder.equal(root.get("rolodexId"), rolodexId);
		} else {
			predicate2 = builder.equal(root.get("tbnId"), tbnId);
		}
		query.where(builder.and(predicate1, predicate2));
		if (!(session.createQuery(query).list()).isEmpty() && (session.createQuery(query).list()) != null) {
			return true;
		} else {
			return false;
		}
	}

	@Override
	public AwardBudgetPerson getBudgetPersonByPersonId(Integer budgetId, String personId, Integer rolodexId,
			String personType, String tbnId) {
		List<AwardBudgetPerson> awardBudgetPerson = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetPerson> query = builder.createQuery(AwardBudgetPerson.class);
		Root<AwardBudgetPerson> rootProposalPersonRoles = query.from(AwardBudgetPerson.class);
		Predicate predicateTwo = builder.equal(rootProposalPersonRoles.get("budgetHeaderId"), budgetId);
		Predicate predicateOne = null;
		if (personType.equals(Constants.EMPLOYEE_PERSON_TYPE)) {
			predicateOne = builder.equal(rootProposalPersonRoles.get("personId"), personId);
		} else if (personType.equals(Constants.NON_EMPLOYEE_TYPE)) {
			predicateOne = builder.equal(rootProposalPersonRoles.get("rolodexId"), rolodexId);
		} else {
			predicateOne = builder.equal(rootProposalPersonRoles.get("tbnId"), tbnId);
		}
		query.where(builder.and(predicateOne, predicateTwo));
		awardBudgetPerson = session.createQuery(query).getResultList();
		if (awardBudgetPerson != null && !awardBudgetPerson.isEmpty()) {
			return awardBudgetPerson.get(0);
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Integer getMaxLineItemNumberByPeriodId(Integer budgetPeriodId) {
		List<AwardBudgetDetail> awardBudgetDetails = fetchAwardBudgetDetailByPeriodId(budgetPeriodId);
		if (awardBudgetDetails != null && !awardBudgetDetails.isEmpty()) {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT MAX(lineItemNumber) FROM AwardBudgetDetail WHERE budgetPeriodId=:budgetPeriodId";
			org.hibernate.query.Query<Integer> query = session.createQuery(hqlQuery);
			query.setParameter("budgetPeriodId", budgetPeriodId);
			return query.uniqueResult();
		} else {
			return 0;
		}
	}

	@Override
	public List<AwardBudgetDetail> getAwardBudgetDetailsByParams(Integer budgetHeaderId, String costElement) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetDetail> query = builder.createQuery(AwardBudgetDetail.class);
		Root<AwardBudgetDetail> root = query.from(AwardBudgetDetail.class);
		Predicate predicate1 = builder.equal(root.get("budgetId"), budgetHeaderId);
		Predicate predicate2 = builder.equal(root.get("costElementCode"), costElement);
		query.where(builder.and(predicate1, predicate2));
		return session.createQuery(query).list();
	}

	@Override
	public AwardRates saveOrUpdateAwardBudgetRate(AwardRates awardRate) {
		hibernateTemplate.saveOrUpdate(awardRate);
		return awardRate;
	}

	@Override
	public List<AwardRates> fetchAwardRatesByBudgetId(Integer budgetId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardRates> query = builder.createQuery(AwardRates.class);
		Root<AwardRates> root = query.from(AwardRates.class);
		Predicate predicate1 = builder.equal(root.get("budgetHeaderId"), budgetId);
		query.where(builder.and(predicate1));
		return session.createQuery(query).list();
	}

	@Override
	public void deleteAwardBudgetRate(List<AwardRates> awardRate) {
		hibernateTemplate.deleteAll(awardRate);
	}
	
	@Override
	public AwardBudgetDetailCalcAmount saveOrUpdateAwardBudgetDetailCalcAmount(AwardBudgetDetailCalcAmount awardBudgetDetailCalcAmount) {
		hibernateTemplate.saveOrUpdate(awardBudgetDetailCalcAmount);
		return awardBudgetDetailCalcAmount;
	}

	@Override
	public List<AwardBudgetDetailCalcAmount> getAwardBudgetCalcAmountByAwdBudgetDetailId(Integer detailId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetDetailCalcAmount> query = builder.createQuery(AwardBudgetDetailCalcAmount.class);
		Root<AwardBudgetDetailCalcAmount> root = query.from(AwardBudgetDetailCalcAmount.class);
		Predicate predicate = builder.equal(root.get("budgetDetailId"), detailId);
		query.where(builder.and(predicate));
		return session.createQuery(query).list();
	}

	@Override
	public void deleteAwardBudgetCalcAmountByAwdBudgetDetailId(AwardBudgetDetailCalcAmount awardBudgetDetailCalcAmount) {
		hibernateTemplate.delete(awardBudgetDetailCalcAmount);
	}

	@Override
	public void deleteAllCalcAmount(List<AwardBudgetDetailCalcAmount> awardBudgetDetailCalcAmounts) {
		hibernateTemplate.deleteAll(awardBudgetDetailCalcAmounts);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Boolean checkAwardBudgetPersonAddedInBudget(Integer budgetPersonId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT COUNT(*) FROM AwardBudgetPersonalDetail WHERE budgetPersonId=:budgetPersonId";
		org.hibernate.query.Query<Long> query = session.createQuery(hqlQuery);
		query.setParameter("budgetPersonId", budgetPersonId);
		Long count = query.getSingleResult();
		if (count > 0) {
			return true;
		}
		return false;
	}

	@Override
	public String getInternalOrderCodeByBudgetDetailId(Integer budgetDetailId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT internalOrderCode FROM AwardBudgetDetail WHERE budgetDetailId=:budgetDetailId";
		javax.persistence.Query query = session.createQuery(hqlQuery);
		query.setParameter("budgetDetailId", budgetDetailId);
		return (String) query.getSingleResult();
	}

	@Override
	public List<AwardBudgetHeader> fetchAwardBudgetHeaderByAwardId(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetHeader> query = builder.createQuery(AwardBudgetHeader.class);
		Root<AwardBudgetHeader> root = query.from(AwardBudgetHeader.class);
		Predicate predicate1 = builder.equal(root.get("awardNumber"), awardNumber);
		query.where(builder.and(predicate1));
		return session.createQuery(query).list();
	}

	@Override
	public AwardBudgetHeader getAwardIdrBySeqNumberAndAwardNumber(Integer seqNumber, String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetHeader> query = builder.createQuery(AwardBudgetHeader.class);
		Root<AwardBudgetHeader> root = query.from(AwardBudgetHeader.class);
		Predicate predicate1 = builder.equal(root.get("awardNumber"), awardNumber);
		Predicate predicate2 = builder.equal(root.get("sequenceNumber"), seqNumber);
		query.where(builder.and(predicate1, predicate2));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public String getIOCodeByBudgetDetailId(Integer budgetDetailId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT T.internalOrderCode FROM AwardBudgetDetail T WHERE T.budgetDetailId = :budgetDetailId";
			javax.persistence.Query query = session.createQuery(hqlQuery);
			query.setParameter("budgetDetailId", budgetDetailId);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			return null;

		}
	}

	@Override
	public InstituteRate fetchInstituteRateByUnitAndDateLessthanMax(Timestamp startDate, String activityTypeCode, String rateClassCode, String rateTypeCode, String unitNumber) {
		InstituteRate instituteRate = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(InstituteRate.class);
		criteria.add(Restrictions.eq("unitNumber", unitNumber));
		criteria.add(Restrictions.eq("activityTypeCode", activityTypeCode));
		criteria.add(Restrictions.eq("rateClassCode", rateClassCode));
		criteria.add(Restrictions.eq("rateTypeCode", rateTypeCode));
		criteria.add(Restrictions.le("startDate", startDate));
		criteria.addOrder(Order.desc("startDate"));
		criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
		@SuppressWarnings("unchecked")
		List<InstituteRate> instituteRates = criteria.list();
		if (instituteRates != null && !instituteRates.isEmpty()) {
			instituteRate = instituteRates.get(0);
		} else {
			String parentUnitNumber = unitHierarchyDao.fetchParentUnitNumberByUnitNumber(unitNumber);
			if (parentUnitNumber != null) {
				instituteRate = fetchInstituteRateByUnitAndDateLessthanMax(startDate, activityTypeCode, rateClassCode, rateTypeCode, parentUnitNumber);
			}
		}
		return instituteRate;
	}

	@Override
	public void deleteBudgetNonPersonDetail(Integer budgetNonPersonDtlId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AwardBudgetNonPersonDetail> delete = builder.createCriteriaDelete(AwardBudgetNonPersonDetail.class);
		Root<AwardBudgetNonPersonDetail> root = delete.from(AwardBudgetNonPersonDetail.class);
		delete.where(builder.equal(root.get("budgetNonPersonDtlId"), budgetNonPersonDtlId));
		session.createQuery(delete).executeUpdate();
	}

	@Override
	public List<AwardBudgetFundType> fetchAllBudgetFundType() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetFundType> query = builder.createQuery(AwardBudgetFundType.class);
		Root<AwardBudgetFundType> rootAgreementSponsorType = query.from(AwardBudgetFundType.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("fundType")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardBudgetFundType getBudgetFundTypeByCode(String availableFundType) {
		return hibernateTemplate.load(AwardBudgetFundType.class, availableFundType);
	}

	@SuppressWarnings("unchecked")
	@Override
	public AwardBudgetHeader getAwardBudgetHeaderWithOutErrorInPosting(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String maxHQLQuery = "SELECT MAX(versionNumber) FROM AwardBudgetHeader WHERE awardId=:awardId and budgetStatusCode <> '11'";
		org.hibernate.query.Query<Integer> countQuery = session.createQuery(maxHQLQuery);
		countQuery.setParameter("awardId", awardId);
		Integer maxVersionNumber = countQuery.uniqueResult();
		if (maxVersionNumber != null) {
			return getBudgetHeaderByVersionNumber(awardId, maxVersionNumber);
		} else {
			return getAwardBudgetHeaderByAwardId(awardId);
		}
	}

	@SuppressWarnings("unchecked")
	private AwardBudgetHeader getBudgetHeaderByVersionNumber(Integer awardId,Integer versionNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM AwardBudgetHeader WHERE awardId=:awardId and versionNumber=:versionNumber";
		org.hibernate.query.Query<AwardBudgetHeader> query = session.createQuery(hqlQuery);
		query.setParameter("awardId", awardId);
		query.setParameter("versionNumber", versionNumber);
		return query.uniqueResult();
	}

	@Override
	public AwardBudgetHeader getLatestAwardBudgetHeader(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetHeader> mainQuery = builder.createQuery(AwardBudgetHeader.class);
		Root<AwardBudgetHeader> rootMainAwardBudgetHeader = mainQuery.from(AwardBudgetHeader.class);
		Subquery<Integer> subQuery = mainQuery.subquery(Integer.class);
		Root<AwardBudgetHeader> rootAwardBudgetHeader = subQuery.from(AwardBudgetHeader.class);
		Subquery<Integer> subQueryOne = mainQuery.subquery(Integer.class);
		Root<AwardBudgetHeader> rootSubAwardBudgetHeader = subQueryOne.from(AwardBudgetHeader.class);
		Subquery<Integer> subQueryTwo = subQueryOne.subquery(Integer.class);
		Root<Award> subRootAward = subQueryTwo.from(Award.class);
		Predicate predicateAwardNumber = builder.equal(subRootAward.get("awardNumber"), awardNumber);
		Predicate predicateSequenceStatus = builder.notEqual(subRootAward.get("awardSequenceStatus"), Constants.CANCELLED_TRANSACTION);
		subQueryTwo.select(subRootAward.get("awardId")).where(builder.and(predicateAwardNumber, predicateSequenceStatus));
		subQueryOne.select(builder.max(rootSubAwardBudgetHeader.get("versionNumber"))).where(builder.in(rootSubAwardBudgetHeader.get("awardId")).value(subQueryTwo));
		subQuery.select(builder.max(rootAwardBudgetHeader.get("budgetId"))).where(builder.and(builder.in(rootAwardBudgetHeader.get("versionNumber")).value(subQueryOne)),builder.in(rootAwardBudgetHeader.get("awardId")).value(subQueryTwo));
		mainQuery.where(builder.in(rootMainAwardBudgetHeader.get("budgetId")).value(subQuery));
		return session.createQuery(mainQuery).uniqueResult();
	}

	@Override
	public AwardBudgetDetail getAwardBudgetDetailsByDetailId(Integer budgetDetailId) {
		return hibernateTemplate.get(AwardBudgetDetail.class, budgetDetailId);
	}

	@Override
	public AwardBudgetPersonalDetail getAwardBudgetPersonalDetailsByPersonDetailId(Integer budgetPersonDetailId) {
		return hibernateTemplate.get(AwardBudgetPersonalDetail.class, budgetPersonDetailId);
	}

	@Override
	public AwardBudgetNonPersonDetail getAwardBudgetNonPersonalDetailsByPersonDetailId(Integer budgetNonPersonDetailId) {
		return hibernateTemplate.get(AwardBudgetNonPersonDetail.class, budgetNonPersonDetailId);
  }
 
	@Override
	public List<Integer> getAwardBudgetVersionBasedOnBatchId(Integer batchId, String awardNumber, String currentBudgetStatusCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Integer> mainQuery = builder.createQuery(Integer.class);
			Root<AwardBudgetHeader> rootMainAwardBudgetHeader = mainQuery.from(AwardBudgetHeader.class);
			Subquery<String> subQuery = mainQuery.subquery(String.class);
			Root<SapAwardFeed> rootSapAwardFeed = subQuery.from(SapAwardFeed.class);
			Predicate predicateAwardNumber = builder.equal(rootSapAwardFeed.get("awardNumber"), awardNumber);
			Predicate predicateBatchId = builder.equal(rootSapAwardFeed.get("batchId"), batchId);
			subQuery.select(rootSapAwardFeed.get("awardId"));
			subQuery.where(builder.and(predicateAwardNumber, predicateBatchId));
			Predicate awardNumberPredicate = builder.equal(rootMainAwardBudgetHeader.get("awardNumber"), awardNumber);
			Predicate budgetStatusCode = builder.equal(rootMainAwardBudgetHeader.get("budgetStatusCode"), currentBudgetStatusCode);
			mainQuery.select(rootMainAwardBudgetHeader.get("versionNumber"));
			Predicate predicateFour = builder.and(awardNumberPredicate, budgetStatusCode, builder.in(rootMainAwardBudgetHeader.get("awardId")).value(subQuery));
			mainQuery.where(builder.and(predicateFour)); 																			 
			return session.createQuery(mainQuery).getResultList();	
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}

	@Override
	public List<Integer> getMasterAwardBudget(String awardNumber, String currentBudgetStatusCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Integer> mainQuery = builder.createQuery(Integer.class);
			Root<AwardBudgetHeader> rootMainAwardBudgetHeader = mainQuery.from(AwardBudgetHeader.class);
			Predicate predicateAwardNumber = builder.equal(rootMainAwardBudgetHeader.get("awardNumber"), awardNumber);
			Predicate predicateBudgetStatus = builder.equal(rootMainAwardBudgetHeader.get("budgetStatusCode"), currentBudgetStatusCode);
			Subquery<Integer> subQueryTwo = mainQuery.subquery(Integer.class);
			Root<Award> rootAward = subQueryTwo.from(Award.class);
			Predicate awardNumberPredicate = builder.equal(rootAward.get("awardNumber"), awardNumber);
			Predicate awardSequenceStatus = builder.equal(rootAward.get("awardDocumentTypeCode"), Constants.MASTER_AWARD);
			subQueryTwo.select(rootAward.get("awardId"));
			subQueryTwo.where(builder.and(awardNumberPredicate, awardSequenceStatus));
			mainQuery.select(rootMainAwardBudgetHeader.get("budgetId"));
			Predicate predicateJoin = builder.and(predicateAwardNumber, predicateBudgetStatus, builder.in(rootMainAwardBudgetHeader.get("awardId")).value(subQueryTwo));
			mainQuery.where(builder.and(predicateJoin)); 																			 
			return session.createQuery(mainQuery).getResultList();	
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}

	@Override
	public void updateAwardBudgetHeadersBasedOnVersionNumbers(List<Integer> versionNumbers, String budgetStatus, String awardNumber, String currentBudgetStatusCode) {																											  
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<AwardBudgetHeader> criteriaUpdate = cb.createCriteriaUpdate(AwardBudgetHeader.class);
		Root<AwardBudgetHeader> rootAwardBudgetHeader = criteriaUpdate.from(AwardBudgetHeader.class);
		criteriaUpdate.set("budgetStatusCode",budgetStatus);
		Predicate predicateVersionNumber = rootAwardBudgetHeader.get("versionNumber").in(versionNumbers);
		Predicate awardNumberPredicate = cb.equal(rootAwardBudgetHeader.get("awardNumber"), awardNumber);
		Predicate predicateBudgetStatus = cb.equal(rootAwardBudgetHeader.get("budgetStatusCode"), currentBudgetStatusCode);
		criteriaUpdate.where(cb.and(predicateVersionNumber, awardNumberPredicate, predicateBudgetStatus)); 																			 
		session.createQuery(criteriaUpdate).executeUpdate();		
	}

	@Override
	public List<Integer> getAwardBudgetVersionBasedOnAwardIdsAndStatus(Set<Integer> awardIds, String awardNumber, String currentBudgetStatusCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Integer> mainQuery = builder.createQuery(Integer.class);
			Root<AwardBudgetHeader> rootMainAwardBudgetHeader = mainQuery.from(AwardBudgetHeader.class);
			Predicate budgetStatusCode = builder.equal(rootMainAwardBudgetHeader.get("budgetStatusCode"), currentBudgetStatusCode);
			Predicate predicateAwardIds = rootMainAwardBudgetHeader.get("awardId").in(awardIds);
			mainQuery.select(rootMainAwardBudgetHeader.get("versionNumber"));
			Predicate predicateFour = builder.and(predicateAwardIds, budgetStatusCode);
			mainQuery.where(builder.and(predicateFour)); 																			 
			return session.createQuery(mainQuery).getResultList();	
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}

	@Override
	public List<AwardBudgetDetail> fetchAwardBudgetDetailByPeriodIds(Set<Integer> budgetPeriodIds) {
		if(!budgetPeriodIds.isEmpty()) {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardBudgetDetail> query = builder.createQuery(AwardBudgetDetail.class);
			Root<AwardBudgetDetail> root = query.from(AwardBudgetDetail.class);
			query.where(root.get("budgetPeriodId").in(budgetPeriodIds));
			return session.createQuery(query).getResultList();
		}
		return new ArrayList<>();
	}

	@Override
	public List<AwardBudgetDetailCalcAmount> getAwardBudgetCalcAmountByAwdBudgetDetailIds(Set<Integer> budgetDetailIds) {
		if(!budgetDetailIds.isEmpty()) {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardBudgetDetailCalcAmount> query = builder.createQuery(AwardBudgetDetailCalcAmount.class);
			Root<AwardBudgetDetailCalcAmount> root = query.from(AwardBudgetDetailCalcAmount.class);
			query.where(root.get("budgetDetailId").in(budgetDetailIds));
			return session.createQuery(query).getResultList();
		}
		return new ArrayList<>();
	}

	@Override
	public List<AwardBudgetPerson> copyBudgetPersonWithoutLineItem(Integer budgetId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetPerson> mainQuery = builder.createQuery(AwardBudgetPerson.class);
		Root<AwardBudgetPerson> rootMainAwardBudgetPerson = mainQuery.from(AwardBudgetPerson.class);
		Predicate predicateBudgetHeader = builder.equal(rootMainAwardBudgetPerson.get("budgetHeaderId"), budgetId);
		Subquery<Integer> subQuery = mainQuery.subquery(Integer.class);
		Root<AwardBudgetPersonalDetail> rootPersonDetail = subQuery.from(AwardBudgetPersonalDetail.class);
		Predicate predicateDetailBudgetHeader = builder.equal(rootPersonDetail.get("budgetDetail").get("budgetId"), budgetId);
		subQuery.select(rootPersonDetail.get("budgetPersonId"));
		subQuery.where(predicateDetailBudgetHeader);
		Predicate predicateNotBudgetHeader = builder.not(rootMainAwardBudgetPerson.get("budgetHeaderId").in(subQuery));
		mainQuery.where(builder.and(predicateNotBudgetHeader, predicateBudgetHeader)); 																			 
		return session.createQuery(mainQuery).getResultList();
	}

	@Override
	public String getNextLineItemSequenceNumber(Integer budgetId, String budgetCategoryCode, String accountNumber) {
		StringBuilder hqlQuery = new StringBuilder();
		String nextSequenceNumber = null;
		String accountNumberWithBudgetCategory = accountNumber.concat(budgetCategoryCode);
		hqlQuery.append("select count(budgetId) from AwardBudgetDetail t1 where");
		hqlQuery.append(" substr(internalOrderCode,1,(length(internalOrderCode)-2)) =:accountNumberWithBudgetCategory and t1.budgetId =:budgetId");
		javax.persistence.Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("accountNumberWithBudgetCategory", accountNumberWithBudgetCategory);
		query.setParameter("budgetId", budgetId);
		Integer count = Integer.parseInt(query.getSingleResult().toString());
		if (count != 0) {
			StringBuilder hqlMaxQuery = new StringBuilder();
			hqlMaxQuery.append("select max(substr(internalOrderCode,-2))+1 from AwardBudgetDetail t1  where t1.budgetId =:budgetId and ");
			hqlMaxQuery.append(" budgetCategoryCode =:budgetCategoryCode and internalOrderCode is not null");
			javax.persistence.Query maxQuery = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlMaxQuery.toString());
			maxQuery.setParameter("budgetId", budgetId);
			maxQuery.setParameter("budgetCategoryCode", budgetCategoryCode);
			nextSequenceNumber = maxQuery.getSingleResult().toString();
		}
		return nextSequenceNumber != null ? nextSequenceNumber : "1";
	}

}
