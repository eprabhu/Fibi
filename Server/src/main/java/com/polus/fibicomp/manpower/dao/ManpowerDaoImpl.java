package com.polus.fibicomp.manpower.dao;

import java.math.BigDecimal;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import javax.persistence.EntityManager;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseTransaction;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.manpower.dto.ManpowerPersonSearchResult;
import com.polus.fibicomp.manpower.pojo.AwardManpower;
import com.polus.fibicomp.manpower.pojo.AwardManpowerPayroll;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.manpower.pojo.ManpowerBudgetReferenceType;
import com.polus.fibicomp.manpower.pojo.ManpowerCandidateTitleType;
import com.polus.fibicomp.manpower.pojo.ManpowerCompensationType;
import com.polus.fibicomp.manpower.pojo.ManpowerConfigurationData;
import com.polus.fibicomp.manpower.pojo.ManpowerInterfaceStatus;
import com.polus.fibicomp.manpower.pojo.ManpowerInterfaceType;
import com.polus.fibicomp.manpower.pojo.ManpowerJobProfileType;
import com.polus.fibicomp.manpower.pojo.ManpowerLogUser;
import com.polus.fibicomp.manpower.pojo.ManpowerPositionStatus;
import com.polus.fibicomp.manpower.pojo.ManpowerResourceType;
import com.polus.fibicomp.manpower.pojo.ManpowerType;
import com.polus.fibicomp.manpower.pojo.ManpowerUpgradeType;
import com.polus.fibicomp.manpower.pojo.WorkdayManpowerInterface;
import com.polus.fibicomp.sectionwiseedit.pojo.ModuleVariableSection;

import oracle.jdbc.internal.OracleTypes;

@Transactional
@Service(value = "manpowerDao")
public class ManpowerDaoImpl implements ManpowerDao {

	protected static Logger logger = LogManager.getLogger(ManpowerDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private EntityManager entityManager;

	@Value("${oracledb}")
	private String oracledb;

	@Override
	public List<ManpowerType> getManpowerTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerType> query = builder.createQuery(ManpowerType.class);
		Root<ManpowerType> rootManpowerType = query.from(ManpowerType.class);
		Predicate predicateManpowerType = builder.equal(rootManpowerType.get("isActive"), Constants.YES);
		query.where(builder.and(predicateManpowerType));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ManpowerBudgetReferenceType> getManpowerBudgetReferenceType() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerBudgetReferenceType> query = builder.createQuery(ManpowerBudgetReferenceType.class);
		Root<ManpowerBudgetReferenceType> rootManpowerBudgetReferenceType = query.from(ManpowerBudgetReferenceType.class);
		Predicate predicateManpowerBudgetReferenceType = builder.equal(rootManpowerBudgetReferenceType.get("isActive"), Constants.YES);
		query.where(builder.and(predicateManpowerBudgetReferenceType));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ManpowerCandidateTitleType> getManpowerCandidateTitleType() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerCandidateTitleType> query = builder.createQuery(ManpowerCandidateTitleType.class);
		Root<ManpowerCandidateTitleType> rootManpowerCandidateTitleType = query.from(ManpowerCandidateTitleType.class);
		Predicate predicateManpowerCandidateTitleType = builder.equal(rootManpowerCandidateTitleType.get("isActive"), Constants.YES);
		query.where(builder.and(predicateManpowerCandidateTitleType));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ManpowerCompensationType> getManpowerCompensationType() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerCompensationType> query = builder.createQuery(ManpowerCompensationType.class);
		Root<ManpowerCompensationType> rootManpowerCompensationType = query.from(ManpowerCompensationType.class);
		Predicate predicateManpowerCompensationType = builder.equal(rootManpowerCompensationType.get("isActive"), Constants.YES);
		query.where(builder.and(predicateManpowerCompensationType));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ManpowerInterfaceStatus> getManpowerInterfaceStatus() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerInterfaceStatus> query = builder.createQuery(ManpowerInterfaceStatus.class);
		Root<ManpowerInterfaceStatus> rootManpowerInterfaceStatus = query.from(ManpowerInterfaceStatus.class);
		Predicate predicateManpowerInterfaceStatus = builder.equal(rootManpowerInterfaceStatus.get("isActive"), Constants.YES);
		query.where(builder.and(predicateManpowerInterfaceStatus));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ManpowerInterfaceType> getManpowerInterfaceType() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerInterfaceType> query = builder.createQuery(ManpowerInterfaceType.class);
		Root<ManpowerInterfaceType> rootManpowerInterfaceType = query.from(ManpowerInterfaceType.class);
		Predicate predicateManpowerInterfaceType = builder.equal(rootManpowerInterfaceType.get("isActive"), Constants.YES);
		query.where(builder.and(predicateManpowerInterfaceType));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ManpowerJobProfileType> getManpowerJobProfileType() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerJobProfileType> query = builder.createQuery(ManpowerJobProfileType.class);
		Root<ManpowerJobProfileType> rootManpowerJobProfileType = query.from(ManpowerJobProfileType.class);
		Predicate predicateIsActive = builder.equal(rootManpowerJobProfileType.get("isActive"), Constants.YES);
		Predicate predicateIsWorkdayActive = builder.equal(rootManpowerJobProfileType.get("isWorkdayActive"), Constants.YES);
		query.where(builder.and(predicateIsActive, predicateIsWorkdayActive));
		query.orderBy(builder.asc(rootManpowerJobProfileType.get("sortOrder")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ManpowerPositionStatus> getManpowerPositionStatus() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerPositionStatus> query = builder.createQuery(ManpowerPositionStatus.class);
		Root<ManpowerPositionStatus> rootManpowerPositionStatus = query.from(ManpowerPositionStatus.class);
		Predicate predicateManpowerPositionStatus = builder.equal(rootManpowerPositionStatus.get("isActive"), Constants.YES);
		query.where(builder.and(predicateManpowerPositionStatus));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ManpowerResourceType> getManpowerResourceType() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerResourceType> query = builder.createQuery(ManpowerResourceType.class);
		Root<ManpowerResourceType> rootManpowerResourceType = query.from(ManpowerResourceType.class);
		Predicate predicateManpowerResourceType = builder.equal(rootManpowerResourceType.get("isActive"), Constants.YES);
		query.where(builder.and(predicateManpowerResourceType));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Boolean checkIfAwardManpowerIsExistBasedOnParams(Integer awardId) {
		List<AwardManpower> awardManpowers = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpower> query = builder.createQuery(AwardManpower.class);
		Root<AwardManpower> root = query.from(AwardManpower.class);
		Predicate predicateAwardId = builder.equal(root.get("awardId"), awardId);
		query.where(builder.and(predicateAwardId));
		awardManpowers = session.createQuery(query).getResultList();
		return awardManpowers != null && !awardManpowers.isEmpty() ? Boolean.TRUE : Boolean.FALSE;
	}

	@Override
	public AwardManpowerResource saveOrUpdateAwardManpowerResources(AwardManpowerResource awardManpowerResource) {
		try {
			hibernateTemplate.saveOrUpdate(awardManpowerResource);
		} catch (Exception e) {
			logger.info("Error ocuured in saveOrUpdateAwardMAnpowerResources {}", e.getMessage());
			throw new ApplicationException("Error occurred in saveOrUpdateAwardManpowerResources", e, Constants.JAVA_ERROR);
		}
		return awardManpowerResource;
	}

	@Override
	public AwardManpower saveOrUpdateAwardManpower(AwardManpower awardManpower) {
		try {
			hibernateTemplate.saveOrUpdate(awardManpower);
		} catch (Exception e) {
			logger.info("Error ocuured in saveOrUpdateAwardManpower {}", e.getMessage());
			throw new ApplicationException("Error occurred in saveOrUpdateAwardManpower", e, Constants.JAVA_ERROR);
		}
		return awardManpower;
	}

	@Override
	public List<AwardManpower> getAwardManpowerDetails(Integer awardId, String manpowerTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpower> query = builder.createQuery(AwardManpower.class);
		Root<AwardManpower> rootAwardManpower = query.from(AwardManpower.class);
		Predicate predicateAwardId = builder.equal(rootAwardManpower.get("awardId"), awardId);
		Predicate predicateManpowerTypeCode = builder.equal(rootAwardManpower.get("manpowerTypeCode"), manpowerTypeCode);
		if (awardId != null && manpowerTypeCode != null) {
			query.where(builder.and(predicateAwardId, predicateManpowerTypeCode));
		} else if(manpowerTypeCode == null) {
			query.where(builder.and(predicateAwardId));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardBudgetDetail getAwardBudgetDetailByParams(Integer awardBudgetDetailId, String wbsNumber, Integer awardBudgetHeaderId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetDetail> query = builder.createQuery(AwardBudgetDetail.class);
		Root<AwardBudgetDetail> rootAwardBudgetDetail = query.from(AwardBudgetDetail.class);
		Predicate predicateAwardBudgetDetailId = builder.equal(rootAwardBudgetDetail.get("budgetDetailId"), awardBudgetDetailId);
		Predicate predicateWbsNumber = builder.equal(rootAwardBudgetDetail.get("internalOrderCode"), wbsNumber);
		Predicate predicateAwardBudgetHeaderId = builder.equal(rootAwardBudgetDetail.get("budgetId"), awardBudgetHeaderId);
		if (awardBudgetDetailId != null) {
			query.where(builder.and(predicateAwardBudgetDetailId));
		} else {
			query.where(builder.and(predicateWbsNumber, predicateAwardBudgetHeaderId));
		}
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public ManpowerType getManpowerTypeBasedOnCode(String manpowerTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerType> query = builder.createQuery(ManpowerType.class);
		Root<ManpowerType> rootManpowerType = query.from(ManpowerType.class);
		query.where(builder.and(builder.equal(rootManpowerType.get("manpowerTypeCode"), manpowerTypeCode)));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public AwardManpower fetchAwardManpowerDetailByAwardManpowerId(Integer awardManpowerId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpower> query = builder.createQuery(AwardManpower.class);
		Root<AwardManpower> rootAwardManpower = query.from(AwardManpower.class);
		query.where(builder.and(builder.equal(rootAwardManpower.get("awardManpowerId"), awardManpowerId)));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public Integer countOfActiveManpowerResource(Integer awardManpowerId) {
		Integer count = 0;
		Query query = entityManager.createNativeQuery("SELECT COUNT(DISTINCT(POSITION_ID)) FROM AWARD_MANPOWER_RESOURCE WHERE PERSON_ID IS NOT NULL AND AWARD_MANPOWER_ID = :awardManpowerId AND PERSON_ID <> '999999999100' AND POSITION_STATUS_CODE NOT IN ('8','5') AND (DATE(UTC_TIMESTAMP) BETWEEN DATE(IFNULL(CHARGE_START_DATE, PLAN_START_DATE)) AND DATE(IFNULL(CHARGE_END_DATE, PLAN_END_DATE)))");
		query.setParameter("awardManpowerId", awardManpowerId);
		count = ((Number) query.getSingleResult()).intValue();
		return count;
	}

	@Override
	public List<AwardManpowerResource> getAwardManpowerResources(Integer awardManpowerId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
		Root<AwardManpowerResource> rootManpowerResources = query.from(AwardManpowerResource.class);
		Predicate predicateManpowerResources = builder.equal(rootManpowerResources.get("awardManpowerId"),awardManpowerId);
		query.where(builder.and(predicateManpowerResources));
		return session.createQuery(query).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardManpower> getBudgetReferenceNumbers(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String budgetReferenceType = "FROM  AwardManpower where awardId =:awardId and budgetReferenceNumber is not null";
		Query query = session.createQuery(budgetReferenceType);
		query.setParameter("awardId", awardId);
		return query.getResultList();
	}

	@Override
	public String deleteManpowerResource(Integer manpowerResourceId) {
		hibernateTemplate.delete(hibernateTemplate.get(AwardManpowerResource.class, manpowerResourceId));
		return "ManpowerResource Delete Successfully";
	}

	@Override
	public Manpower fetchManpowerPersonDetail(String positionId, String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Manpower> query = builder.createQuery(Manpower.class);
		Root<Manpower> rootManpower = query.from(Manpower.class);
		Predicate predicatePositionId = builder.equal(rootManpower.get("positionId"), positionId);
		Predicate predicatePersonId = builder.equal(rootManpower.get("manpowerPersonId"), personId);
		if (positionId != null && personId != null) {
			query.where(builder.and(predicatePositionId, predicatePersonId));
		} else if (positionId != null) {
			query.where(builder.and(predicatePositionId));
		} else if (personId != null) {
			query.where(builder.and(predicatePersonId));
		}
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public ManpowerPositionStatus getManpowerPositionStatusById(String positionStatusCode) {
		return hibernateTemplate.get(ManpowerPositionStatus.class, positionStatusCode);
	}

	@Override
	public ManpowerCompensationType getManpowerPlanCompensationTypeById(String planCompensationTypeCode) {
		return hibernateTemplate.get(ManpowerCompensationType.class, planCompensationTypeCode);
	}

	@Override
	public ManpowerJobProfileType getManpowerJobProfileTypeById(String jobProfileTypeCode) {
		return hibernateTemplate.get(ManpowerJobProfileType.class, jobProfileTypeCode);
	}

	@Override
	public WorkdayManpowerInterface saveOrUpdateWorkdayManpowerInterface(WorkdayManpowerInterface workdayManpowerInterface) {
		try {
			hibernateTemplate.saveOrUpdate(workdayManpowerInterface);
		} catch (Exception e) {
			logger.info("Error ocuured in saveOrUpdateWorkdayManpowerInterface {}", e.getMessage());
		}
		return workdayManpowerInterface;
	}

	@Override
	public AwardManpowerResource getAwardManpowerResourceById(Integer awardManpowerResourceId) {
		return hibernateTemplate.get(AwardManpowerResource.class, awardManpowerResourceId);
	}

	@Override
	public ManpowerResourceType getManpowerResourceTypeById(String resourceTypeCode) {
		return hibernateTemplate.get(ManpowerResourceType.class, resourceTypeCode);
	}

	@Override
	public ManpowerCandidateTitleType getManpowerCandidateTitleTypeById(String candidateTitleTypeCode) {
		return hibernateTemplate.get(ManpowerCandidateTitleType.class, candidateTitleTypeCode);
	}

	@Override
	public BigDecimal fetchAwardBudgetDetailsLineItemCostByCategory(Integer budgetDetailId, String internalOrderCode, Integer budgetId) {
		BigDecimal lineitemCost = BigDecimal.ZERO;
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String lineItemCost = "select lineItemCost from AwardBudgetDetail where budgetId =: budgetId and internalOrderCode = :internalOrderCode or budgetDetailId = :budgetDetailId";
			Query query = session.createQuery(lineItemCost);
			query.setParameter("budgetDetailId", budgetDetailId);
			query.setParameter("internalOrderCode", internalOrderCode);
			query.setParameter("budgetId", budgetId);
			lineitemCost = (BigDecimal) query.getSingleResult();
		} catch (Exception e) {
			logger.info("Error ocuured in fetchAwardBudgetDetailsLineItemCostByCategory {}", e.getMessage());
		}
		return lineitemCost;
	}

	@Override
	public List<AwardManpowerPayroll> fetchManpowerPayrollDetails(String internalOrderCode, String employeeNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpowerPayroll> query = builder.createQuery(AwardManpowerPayroll.class);
		Root<AwardManpowerPayroll> rootAwardManpowerPayroll = query.from(AwardManpowerPayroll.class);
		Predicate predicateInternalOrderCode = builder.equal(rootAwardManpowerPayroll.get("internalOrderCode"), internalOrderCode);
		Predicate predicatePersonId = builder.equal(rootAwardManpowerPayroll.get("employeeNumber"), employeeNumber);
		query.where(builder.and(predicateInternalOrderCode, predicatePersonId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public BigDecimal getSumOfResourcesCommittedAmount(Integer awardManpowerId, Integer manpowerResourceId) {
		BigDecimal committedCost = BigDecimal.ZERO;
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			if (manpowerResourceId != null) {
				String committedAmount = "SELECT sum(committedCost) FROM  AwardManpowerResource where awardManpowerId =:awardManpowerId and manpowerResourceId =:manpowerResourceId";
				Query query = session.createQuery(committedAmount);
				query.setParameter("awardManpowerId", awardManpowerId);
				query.setParameter("manpowerResourceId", manpowerResourceId);
				committedCost = (BigDecimal) query.getSingleResult();
			} else {
				String committedAmount = "SELECT sum(committedCost) FROM  AwardManpowerResource where awardManpowerId =:awardManpowerId";
				Query query = session.createQuery(committedAmount);
				query.setParameter("awardManpowerId", awardManpowerId);
				committedCost = (BigDecimal) query.getSingleResult();
			}
		} catch (Exception e) {
			logger.info("Error ocuured in getSumOfResourcesCommittedAmount {}", e.getMessage());
		}
		return committedCost;
	}

	@Override
	public AwardManpower fetchAwardManpowerDetails(String budgetReferenceNumber, Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpower> query = builder.createQuery(AwardManpower.class);
		Root<AwardManpower> rootAwardManpower = query.from(AwardManpower.class);
		Predicate predicateBudgetReferenceNumber = builder.equal(rootAwardManpower.get("budgetReferenceNumber"), budgetReferenceNumber);
		Predicate predicateAwardId = builder.equal(rootAwardManpower.get("awardId"), awardId);
		if (budgetReferenceNumber != null) {
			query.where(builder.and(predicateBudgetReferenceNumber, predicateAwardId));
		} else {
			query.where(builder.and(predicateAwardId));
		}
		return session.createQuery(query).uniqueResult();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ManpowerPersonSearchResult> findPersonWithPositionId(String searchString) {
		StringBuilder hqlQuery = new StringBuilder();
		String likeCriteria = new StringBuilder().append("%").append(searchString).append("%").toString();
		hqlQuery.append("SELECT distinct NEW com.polus.fibicomp.manpower.dto.ManpowerPersonSearchResult(T2.personId, T2.fullName, T2.principalName, T1.positionId)");
	    hqlQuery.append(" FROM Manpower T1 inner join Person T2 on T2.personId = T1.manpowerPersonId and T2.isGraduateStudentStaff ='N'");
	    hqlQuery.append(" left join  AwardManpowerResource T3 on T3.personId = T1.manpowerPersonId WHERE T2.personId is not null and T2.principalName like :likeCriteria OR T2.fullName like :likeCriteria");
		Query findPersonWithPositionId = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		findPersonWithPositionId.setParameter("likeCriteria", likeCriteria);
		return findPersonWithPositionId.setMaxResults(25).getResultList();
	}

	@Override
	public Boolean checkIfAwardManpowerResourceIsExistBasedOnParams(Integer awardManpowerId) {
		List<AwardManpowerResource> awardManpowerResources = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
		Root<AwardManpowerResource> root = query.from(AwardManpowerResource.class);
		Predicate predicateAwardManpowerId = builder.equal(root.get("awardManpowerId"), awardManpowerId);
		query.where(builder.and(predicateAwardManpowerId));
		awardManpowerResources = session.createQuery(query).getResultList();
		return awardManpowerResources != null && !awardManpowerResources.isEmpty() ? Boolean.TRUE : Boolean.FALSE;
	}

	@Override
	public String deleteAwardManpower(Integer awardManpowerId) {
		hibernateTemplate.delete(hibernateTemplate.get(AwardManpower.class, awardManpowerId));
		return "Manpower Delete Successfully";
	}

	@Override
	public List<AwardExpenseTransaction> getAwardExpenseTransactionDetails(String fiGlAccount, String internalOrderCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardExpenseTransaction> query = builder.createQuery(AwardExpenseTransaction.class);
		Root<AwardExpenseTransaction> rootAwardExpenseTransaction = query.from(AwardExpenseTransaction.class);
		Predicate predicateFiGlAccount = builder.equal(rootAwardExpenseTransaction.get("fiGlAccount"), fiGlAccount);
		Predicate predicateInternalOrderCode = builder.equal(rootAwardExpenseTransaction.get("internalOrderCode"), internalOrderCode);
		query.where(builder.and(predicateFiGlAccount, predicateInternalOrderCode));
		return session.createQuery(query).getResultList();
	}

	@Override
	public BigDecimal getsumOfPayrollAmount(String internalOrderCode, String employeeNumber) {
		BigDecimal payrollAmount = BigDecimal.ZERO;
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String sumOfPayrollAmount = "SELECT sum(amount) FROM  AwardManpowerPayroll where internalOrderCode =:internalOrderCode and employeeNumber =:employeeNumber";
			Query query = session.createQuery(sumOfPayrollAmount);
			query.setParameter("internalOrderCode", internalOrderCode);
			query.setParameter("employeeNumber", employeeNumber);
			payrollAmount = (BigDecimal) query.getSingleResult();
		} catch (Exception e) {
			logger.info("Error ocuured in getsumOfPayrollAmount {}", e.getMessage());
		}
		return payrollAmount;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> getDistinctPositionIds(Integer awardId) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select distinct t2.positionId from AwardManpower t1 left join AwardManpowerResource t2 on t2.awardManpowerId = t1.awardManpowerId where t2.positionId is not null and t1.awardId =:awardId and t2.positionOwnedByAward = 'Y'");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("awardId",awardId);
			return query.getResultList();
		} catch (Exception e) {
			logger.error("Error in getDistinctPositionIds {}", e.getMessage());
			return Collections.emptyList();          
		}		
	}

	@Override
	public BigDecimal getSumOfResourcesPlannedSalary(Integer awardManpowerId, Integer manpowerResourceId) {
		BigDecimal plannedSalary = BigDecimal.ZERO;
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			if(manpowerResourceId != null) {
				String sumOfPlannedSalary = "SELECT sum(plannedSalary) FROM  AwardManpowerResource where awardManpowerId =:awardManpowerId and manpowerResourceId =:manpowerResourceId";
				Query query = session.createQuery(sumOfPlannedSalary);
				query.setParameter("awardManpowerId", awardManpowerId);
				query.setParameter("manpowerResourceId", manpowerResourceId);
				plannedSalary = (BigDecimal) query.getSingleResult();
			} else {
				String sumOfPlannedSalary = "SELECT sum(plannedSalary) FROM  AwardManpowerResource where awardManpowerId =:awardManpowerId";
				Query query = session.createQuery(sumOfPlannedSalary);
				query.setParameter("awardManpowerId", awardManpowerId);
				plannedSalary = (BigDecimal) query.getSingleResult();
			}
		} catch (Exception e) {
			logger.info("Error ocuured in getSumOfResourcesPlannedSalary {}", e.getMessage());
		}
		return plannedSalary;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ManpowerJobProfileType> getManpowerJobProfile(String searchString, String costElementCode) {
		StringBuilder hqlQuery = new StringBuilder();
		String likeCriteria = new StringBuilder().append("%").append(searchString).append("%").toString();
		hqlQuery.append("SELECT distinct NEW com.polus.fibicomp.manpower.pojo.ManpowerJobProfileType(T1.jobProfileTypeCode, T1.description, T1.jobFamily) FROM ManpowerJobProfileType T1");
		hqlQuery.append(" inner join CostElementJobProfileMapping T2 on T1.jobProfileTypeCode = T2.jobProfileCode");
		hqlQuery.append(" WHERE T2.costElementCode = :costElementCode and T1.isActive = 'Y' and T1.isWorkdayActive = 'Y' and (T1.jobProfileTypeCode like :likeCriteria OR  T1.description like :likeCriteria) order by T1.sortOrder asc");
		Query queryManpowerJobProfile = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryManpowerJobProfile.setParameter("likeCriteria", likeCriteria);
		queryManpowerJobProfile.setParameter("costElementCode", costElementCode);
		return queryManpowerJobProfile.getResultList();
	}

	@Override
	public List<AwardManpowerResource> deleteAllManpowerResource(List<AwardManpowerResource> awardManpowerResource) {
		hibernateTemplate.deleteAll(awardManpowerResource);
		return awardManpowerResource;
	}

	@Override
	public List<AwardManpower> deleteAllAwardManpower(List<AwardManpower> awardManpower) {
		hibernateTemplate.deleteAll(awardManpower);
		return awardManpower;
	}

	@Override
	public List<WorkdayManpowerInterface> deleteAllResourcesInWorkday(List<WorkdayManpowerInterface> workdayManpowerInterface) {
		hibernateTemplate.deleteAll(workdayManpowerInterface);
		return workdayManpowerInterface;
	}

	@Override
	public List<WorkdayManpowerInterface> getAwardManpowerWorkday(String awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<WorkdayManpowerInterface> query = builder.createQuery(WorkdayManpowerInterface.class);
		Root<WorkdayManpowerInterface> rootAwardExpenseTransaction = query.from(WorkdayManpowerInterface.class);
		Predicate predicateAwardId = builder.equal(rootAwardExpenseTransaction.get("awardId"), awardId);
		query.where(builder.and(predicateAwardId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardManpowerResource> getAllManpowerResources(Set<Integer> awardManpowerIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		List<String> statusCodes = new ArrayList<>();
		statusCodes.add(Constants.MANPOWER_READY_TO_TRIGGER_POSITION);
		statusCodes.add(Constants.MANPOWER_ACTIVE);
		statusCodes.add(Constants.MANPOWER_POSITION_GENERATED);
		statusCodes.add(Constants.MANPOWER_PENDING_APPROVAL);
		statusCodes.add(Constants.MANPOWER_HIRING_ON_EXISTING_POSITION);
		CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
		Root<AwardManpowerResource> rootAwardManpowerResource = query.from(AwardManpowerResource.class);
		Predicate predicateOne = rootAwardManpowerResource.get("awardManpowerId").in(awardManpowerIds);
		Predicate predicateTwo = rootAwardManpowerResource.get("positionStatusCode").in(statusCodes);
		query.where(builder.and(predicateOne, predicateTwo));
		query.orderBy(builder.asc(rootAwardManpowerResource.get("resourceUniqueId")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardManpower> getAwardManpowerDetailsBasedTypeCodes(Integer awardId, String manpowerTypeCode) {
		try {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpower> query = builder.createQuery(AwardManpower.class);
		Root<AwardManpower> rootAwardManpower = query.from(AwardManpower.class);
		Predicate predicateAwardId = builder.equal(rootAwardManpower.get("awardId"), awardId);
		Predicate predicateManpowerTypeCode = builder.equal(rootAwardManpower.get("manpowerTypeCode"), manpowerTypeCode);
		query.where(builder.and(predicateAwardId, predicateManpowerTypeCode));
		return session.createQuery(query).getResultList();
		} catch (Exception e) {
			logger.error("error in getAwardManpowerDetailsBasedTypeCodes {}", e.getMessage());
			return null;
		}
	}

	@Override
	public Boolean checkIfAwardManpowerResourceIsExistInWorkday(Integer awardManpowerResourceId) {
		List<WorkdayManpowerInterface> workdayManpowerInterface = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<WorkdayManpowerInterface> query = builder.createQuery(WorkdayManpowerInterface.class);
		Root<WorkdayManpowerInterface> root = query.from(WorkdayManpowerInterface.class);
		Predicate predicateAwardManpowerResourceId = builder.equal(root.get("awardManpowerResourceId"), awardManpowerResourceId);
		Predicate predicateInterfaceStatusCode = builder.equal(root.get("interfaceStatusCode"), Constants.MANPOWER_INTERFACE_SUCCESS);
		query.where(builder.and(predicateAwardManpowerResourceId, predicateInterfaceStatusCode));
		workdayManpowerInterface = session.createQuery(query).getResultList();
		return workdayManpowerInterface != null && !workdayManpowerInterface.isEmpty() ? Boolean.TRUE : Boolean.FALSE;
	}

	@Override
	public String getAwardManpowerCostAllocation(String personId, BigDecimal costAllocation, Timestamp startDate, Timestamp endDate, String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		String result = "N";
		try {
			String functionName = "FN_AWD_MANPOWER_COST_ALLOCATN";
			String functionCall = "{ ? = call " + functionName + "(?,?,?,?,?) }";
			statement = connection.prepareCall(functionCall);
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setString(2, personId);
			statement.setBigDecimal(3, costAllocation);
			statement.setTimestamp(4, startDate);
			statement.setTimestamp(5, endDate);
			statement.setString(6, awardNumber);
			statement.execute();
			return statement.getString(1);
		} catch (SQLException e) {
			logger.error("error in getAwardManpowerCostAllocation {}", e.getMessage());
		}
		return result;
	}

	@Override
	public BigDecimal getSumOfPlannedSalary(Integer awardManpowerId) {
		BigDecimal plannedSalary = BigDecimal.ZERO;
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String sumOfPlannedSalary = "SELECT sum(plannedSalary) FROM  AwardManpowerResource where awardManpowerId =:awardManpowerId and committedCost is null";
			Query query = session.createQuery(sumOfPlannedSalary);
			query.setParameter("awardManpowerId", awardManpowerId);
			plannedSalary = (BigDecimal) query.getSingleResult();
		} catch (Exception e) {
			logger.info("Error ocuured in getSumOfResourcesPlannedSalary {}", e.getMessage());
		}
		return plannedSalary == null ? BigDecimal.ZERO : plannedSalary;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ManpowerPersonSearchResult> findGraduateStudents(String searchString) {
		StringBuilder hqlQuery = new StringBuilder();
		String likeCriteria = new StringBuilder().append("%").append(searchString).append("%").toString();
		hqlQuery.append("SELECT distinct NEW com.polus.fibicomp.manpower.dto.ManpowerPersonSearchResult(personId, fullName, principalName)");
		hqlQuery.append(" FROM Person WHERE isGraduateStudentStaff ='Y' and (principalName like :likeCriteria OR fullName like :likeCriteria) and status='A'");
		Query findPerson = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		findPerson.setParameter("likeCriteria", likeCriteria);
		return findPerson.setMaxResults(25).getResultList();
	}

	@Override
	public ManpowerConfigurationData getManpowerConfigurationValue(String configurationKey) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerConfigurationData> query = builder.createQuery(ManpowerConfigurationData.class);
		Root<ManpowerConfigurationData> manpowerConfigurationKey = query.from(ManpowerConfigurationData.class);
		Predicate predicateConfigurationKey = builder.equal(manpowerConfigurationKey.get("configurationKey"), configurationKey);
		query.where(builder.and(predicateConfigurationKey));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public BigDecimal getSumOfCommittedAmount(Integer awardManpowerId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String committedAmount = "SELECT sum(committedCost) FROM  AwardManpowerResource where awardManpowerId =:awardManpowerId";
			Query query = session.createQuery(committedAmount);
			query.setParameter("awardManpowerId", awardManpowerId);
			return (BigDecimal) query.getSingleResult() != null ? (BigDecimal) query.getSingleResult() : BigDecimal.ZERO;
		} catch (Exception e) {
			logger.info("Error ocuured in getSumOfCommittedAmount {}", e.getMessage());
			return BigDecimal.ZERO;
		}
	}

	@Override
	public BigDecimal getSumOfInitialCommittedAmount(Integer awardManpowerId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String plannedSalary = "SELECT sum(plannedSalary) from AwardManpowerResource where awardManpowerId =:awardManpowerId and positionStatusCode <> '6'";
			Query query = session.createQuery(plannedSalary);
			query.setParameter("awardManpowerId", awardManpowerId);
			return (BigDecimal) query.getSingleResult() != null ? (BigDecimal) query.getSingleResult() : BigDecimal.ZERO;
		} catch (Exception e) {
			logger.info("Error ocuured in getSumOfInitialAmount {}", e.getMessage());
			return BigDecimal.ZERO;
		}
	}

	@Override
	public void updateManpowerResourceUniqueId(String resourceUniqueId, Integer manpowerResourceId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update AwardManpowerResource set resourceUniqueId = :resourceUniqueId where manpowerResourceId = :manpowerResourceId");
		query.setParameter("resourceUniqueId", resourceUniqueId);
		query.setParameter("manpowerResourceId", manpowerResourceId);
		query.executeUpdate();
	}

	@Override
	public Boolean checkPositionIdExistInAwardManpower(Integer awardId, String positionId, String personId) {
		StringBuilder hqlQuery = new StringBuilder();
		Query query = null;
		Long count = null;
		try {
		if (positionId != null) {
			hqlQuery.append("SELECT count(*) FROM AwardManpower T1 left join AwardManpowerResource T2 on T2.awardManpowerId = T1.awardManpowerId WHERE T1.awardId = :awardId and T2.positionId =:positionId and T2.positionOwnedByAward = 'Y'");
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("awardId", awardId);
			query.setParameter("positionId", positionId);
		} else if(personId != null) {
			hqlQuery.append("SELECT count(*) FROM AwardManpower T1 left join AwardManpowerResource T2 on T2.awardManpowerId = T1.awardManpowerId WHERE T1.awardId = :awardId and T2.personId =:personId and T2.positionOwnedByAward = 'Y'");
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("awardId", awardId);
			query.setParameter("personId", personId);
		}
		if (query != null) {
			count = (Long) query.getSingleResult();
		}
		return count != null && count > 0;
		} catch(Exception e) {
			logger.info("Error ocuured in checkPositionIdExistInAwardManpower {}", e.getMessage());
			return Boolean.FALSE;	
		}
	}

	@Override
	public List<AwardManpowerResource> getManpowerResourceBasedCostAllocation(String personId, Timestamp startDate, Timestamp endDate, String awardNumber, String resourceFlag) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		List<AwardManpowerResource> awardManpowerResources = new ArrayList<>();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_MANPWR_RSRCE_COST_ALLOCATN(?,?,?,?,?)}");
				statement.setString(1, personId);
				statement.setTimestamp(2, startDate);
				statement.setTimestamp(3, endDate);
				statement.setString(4, awardNumber);
				statement.setString(5, resourceFlag);
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_MANPWR_RSRCE_COST_ALLOCATN";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, personId);
				statement.setTimestamp(3, startDate);
				statement.setTimestamp(4, endDate);
				statement.setString(5, awardNumber);
				statement.setString(6, resourceFlag);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
			while (rset.next()) {
				AwardManpowerResource resource = new AwardManpowerResource((String) rset.getString("POSITION_ID"), ((BigDecimal) rset.getBigDecimal("COST_ALLOCATION")), (String) rset.getString("AWARD_NUMBER"), (String) rset.getString("ACCOUNT_NUMBER"));
				awardManpowerResources.add(resource);
			}
		} catch (SQLException e) {
			logger.error("Exception in getManpowerResourceBasedCostAllocation: {} ", e.getMessage());
		}
		return awardManpowerResources;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ManpowerPersonSearchResult> findManpowerPerson(Integer awardId, String searchString, String manpowerRequestType) {
		StringBuilder hqlQuery = new StringBuilder();
		String likeCriteria = new StringBuilder().append("%").append(searchString).append("%").toString();
	    if (manpowerRequestType.equals(Constants.MANPOWER_UPGRADE_NEW_PROJECT_TYPE_CODE)) {
	    	hqlQuery.append("SELECT NEW com.polus.fibicomp.manpower.dto.ManpowerPersonSearchResult(T1.personId, T1.fullName, T1.principalName)");
	    	hqlQuery.append(" FROM Person T1  where (T1.isGraduateStudentStaff ='N' or T1.isGraduateStudentStaff is null) and (T1.principalName like :likeCriteria OR T1.fullName like :likeCriteria) and T1.status='A'");
	    } else if(manpowerRequestType.equals(Constants.MANPOWER_UPGRADE_PARTIAL_ALLOC_TYPE_CODE)){
	    	hqlQuery.append("SELECT NEW com.polus.fibicomp.manpower.dto.ManpowerPersonSearchResult(T1.manpowerPersonId, T2.fullName, T2.principalName, T1.positionId, T3.jobProfileTypeCode, T3.description)");
		    hqlQuery.append(" FROM Manpower T1 left join Person T2 on T2.personId = T1.manpowerPersonId left join ManpowerJobProfileType T3 on T3.jobProfileTypeCode = T1.jobCode where (T2.isGraduateStudentStaff ='N' or T2.isGraduateStudentStaff is null)");
		    hqlQuery.append(" and T1.positionId is not null and T1.manpowerPersonId is not null and (T2.principalName like :likeCriteria OR T2.fullName like :likeCriteria) and T2.status='A'");
	    } else {
	    	hqlQuery.append("SELECT distinct NEW com.polus.fibicomp.manpower.dto.ManpowerPersonSearchResult(T3.manpowerPersonId, T4.fullName, T4.principalName, T3.positionId, T5.jobProfileTypeCode, T5.description)");
		    hqlQuery.append(" FROM  AwardManpower T1   left join AwardManpowerResource T2 on T2.awardManpowerId = T1.awardManpowerId");
			hqlQuery.append(" LEFT JOIN Manpower T3 on T3.manpowerPersonId = T2.personId left join Person T4 on T4.personId = T3.manpowerPersonId left join ManpowerJobProfileType T5 on T5.jobProfileTypeCode = T3.jobCode where ");
	 	    hqlQuery.append(" T2.positionOwnedByAward ='Y' and T2.positionStatusCode not in('1','2') and T1.awardId =:awardId and T2.personId is not null and (T4.principalName like :likeCriteria OR T4.fullName like :likeCriteria) and T4.status='A'");
	    }
		Query findManpowerPerson = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		if (!manpowerRequestType.equals(Constants.MANPOWER_UPGRADE_NEW_PROJECT_TYPE_CODE) && !manpowerRequestType.equals(Constants.MANPOWER_UPGRADE_PARTIAL_ALLOC_TYPE_CODE)) {
			 findManpowerPerson.setParameter("awardId", awardId);
		}
		findManpowerPerson.setParameter("likeCriteria", likeCriteria);
		return findManpowerPerson.setMaxResults(25).getResultList();
	}

	@Override
	public List<AwardManpowerResource> getResourceJobProfileByParam(String personId, String positionId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
		Root<AwardManpowerResource> rootManpowerResources = query.from(AwardManpowerResource.class);
		Predicate predicatePersonId = builder.equal(rootManpowerResources.get("personId"),personId);
		Predicate predicatePositionId = builder.equal(rootManpowerResources.get("positionId"),positionId);
		query.where(builder.and(predicatePersonId, predicatePositionId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Award getAwardByResourceId(Integer manpowerResourceId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select T1 From Award T1 inner join AwardManpower T2 on T2.awardId = T1.awardId");
		hqlQuery.append("  where T2.awardManpowerId = (select awardManpowerId from AwardManpowerResource where manpowerResourceId =:manpowerResourceId)");
		Query findAward = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		findAward.setParameter("manpowerResourceId", manpowerResourceId);
		return (Award) findAward.getSingleResult();
	}

	@Override
	public boolean isManpowerSectionEditable(String moduleItemKey, String subModuleItemKey, Integer moduleCode, Integer subModuleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		List<String> sectionCodes = new ArrayList<>();
		sectionCodes.add(Constants.MANPOWER_EDITABLE_FIELD);
		sectionCodes.add(Constants.MANPOWER_TRIGGER_RESOURCE_EDITABLE_FIELD);
		CriteriaQuery<ModuleVariableSection> query = builder.createQuery(ModuleVariableSection.class);
		Root<ModuleVariableSection> rootModuleVariableSection = query.from(ModuleVariableSection.class);
		Predicate predicateModuleItemKey = builder.equal(rootModuleVariableSection.get("moduleItemKey"), moduleItemKey);
		Predicate predicateModuleCode = builder.equal(rootModuleVariableSection.get("moduleCode"), moduleCode);
		Predicate predicateSubModuleCode = builder.equal(rootModuleVariableSection.get("subModuleCode"), subModuleCode);
		Predicate predicateSubModuleItemKey = builder.equal(rootModuleVariableSection.get("subModuleItemKey"), subModuleItemKey);
		Predicate predicateBudget = rootModuleVariableSection.get("sectionCode").in(sectionCodes);
		query.where(builder.and(predicateModuleItemKey, predicateModuleCode, predicateSubModuleCode, predicateSubModuleItemKey, predicateBudget));
		return (session.createQuery(query).getResultList() != null && !session.createQuery(query).getResultList().isEmpty());
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardManpowerResource> getActiveManpowerResources() {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select T3 From Award T1 inner join AwardManpower T2 on T2.awardId = T1.awardId");
		hqlQuery.append(" inner join AwardManpowerResource T3 on T3.awardManpowerId = T2.awardManpowerId");
		hqlQuery.append("  where T1.awardSequenceStatus in ('ACTIVE','PENDING') and T3.positionStatusCode <> '8' and T3.chargeStartDate is not null and T3.chargeEndDate is not null");
		Query findManpower = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		return findManpower.getResultList();
	}

	@SuppressWarnings("unchecked")
	public List<Integer> getAwardManpowerResourcesByParam(Integer awardManpowerId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select manpowerResourceId From AwardManpowerResource where awardManpowerId = :awardManpowerId");
		Query findManpower = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		findManpower.setParameter("awardManpowerId", awardManpowerId);
		return findManpower.getResultList();
	}

	@Override
	public BigDecimal getResourceMultiplierUsed(Integer manpowerResourceId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select multiplierValueUsed From AwardManpowerResource where manpowerResourceId = :manpowerResourceId");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("manpowerResourceId", manpowerResourceId);
		return (BigDecimal) query.getSingleResult();
	}

	@Override
	public List<ManpowerUpgradeType> getManpowerUpgradeType() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerUpgradeType> query = builder.createQuery(ManpowerUpgradeType.class);
		Root<ManpowerUpgradeType> rootManpowerUpgradeType = query.from(ManpowerUpgradeType.class);
		Predicate predicateManpowerBudgetReferenceType = builder.equal(rootManpowerUpgradeType.get("isActive"), Boolean.TRUE);
		query.where(builder.and(predicateManpowerBudgetReferenceType));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ManpowerInterfaceType fetchManpowerInterfaceById(String manpowerInterfaceTypeCode) {
		return hibernateTemplate.get(ManpowerInterfaceType.class, manpowerInterfaceTypeCode);
	}

	@Override
	public List<ManpowerInterfaceType> fetchManpowerInterfacesByIds(List<String> manpowerInterfaceTypeCodes) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();						
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerInterfaceType> criteria = builder.createQuery(ManpowerInterfaceType.class);
		Root<ManpowerInterfaceType> root = criteria.from(ManpowerInterfaceType.class);
		criteria.where(root.get("interfaceTypeCode").in(manpowerInterfaceTypeCodes));
		return session.createQuery(criteria).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardManpowerResource> fetchManpowerBaseSalaryDetailsByAwardNumberAndPersonId(String awardNumber, String personId, String accountNumber) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("select new AwardManpowerResource (t1.personId, t1.positionId, t1.fullName, t4.emailAddress, t1.costAllocation, t1.chargeStartDate, t1.chargeEndDate, t1.committedCost, t3.awardNumber, t1.multiplierValueUsed, t1.baseSalaryUsed, t3.accountNumber)");
		hqlQuery.append(" from AwardManpowerResource t1 inner join AwardManpower t2 on t2.awardManpowerId = t1.awardManpowerId inner join Award t3 on t3.awardId = t2.awardId inner join Person t4 on t4.personId = t1.personId");
		hqlQuery.append("  where t2.manpowerTypeCode = 1 and t3.awardSequenceStatus = 'ACTIVE' and (t3.awardNumber =:awardNumber or t3.accountNumber =:accountNumber) and t4.personId =:personId");
		Query queryAwardManpowerResource = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryAwardManpowerResource.setParameter("personId", personId);
		queryAwardManpowerResource.setParameter("awardNumber", awardNumber);
		queryAwardManpowerResource.setParameter("accountNumber", accountNumber);
		return queryAwardManpowerResource.getResultList();
	}

	@Override
	public void saveManpowerLogUserDetails(ManpowerLogUser manpowerLogUser) {
		hibernateTemplate.saveOrUpdate(manpowerLogUser);	
	}

	@Override
	public List<AwardBudgetDetail> getAwardBudgetDetailForManpower(Integer budgetId, List<String> wbsNumbers, List<String> budgetDetailsIds) {
 		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBudgetDetail> query = builder.createQuery(AwardBudgetDetail.class);
		Root<AwardBudgetDetail> root = query.from(AwardBudgetDetail.class);
		Predicate predicateWbsNumber = builder.not(root.get("internalOrderCode").in(wbsNumbers));
		Predicate predicateBudgetDetailId = builder.not(root.get("budgetDetailId").in(budgetDetailsIds));
		Predicate predicateBudgetId = builder.equal(root.get("budgetId"), budgetId);
		Predicate predicateBudgetType = root.get("budgetCategoryCode").in(Constants.BUDGET_CATEGORY_CODE_EOM, Constants.BUDGET_CATEGORY_CODE_RSS);
		if (!wbsNumbers.isEmpty()) {
			query.where(builder.and(predicateWbsNumber, predicateBudgetId, predicateBudgetType));
		} else if (!budgetDetailsIds.isEmpty()) {
			query.where(builder.and(predicateBudgetDetailId, predicateBudgetId, predicateBudgetType));
		} else {
			query.where(builder.and(predicateBudgetId, predicateBudgetType));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardBudgetHeader getAwardBudgetByVersionNumber(Integer awardId) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select new AwardBudgetHeader (budgetId, versionNumber) from AwardBudgetHeader ");
			hqlQuery.append(" where versionNumber = (select max(versionNumber) from AwardBudgetHeader where awardId =:awardId) and awardId =:awardId");
			Query queryAwardBudgetHeader = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryAwardBudgetHeader.setParameter("awardId", awardId);
			return (AwardBudgetHeader) queryAwardBudgetHeader.getSingleResult();
		} catch (Exception e) {
			logger.error("error in getAwardBudgetByVersionNumber{}", e.getMessage());
			return null;
		}
	}

  @SuppressWarnings("unchecked")
	@Override
	public List<AwardManpowerResource> fetchAwardManpowerResourcesByAwardId(Integer awardId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select T1 from AwardManpowerResource T1 ");
		hqlQuery.append(" inner join AwardManpower T2 on T2.awardManpowerId = T1.awardManpowerId ");
		hqlQuery.append(" inner join Award T3 on T3.awardId = T2.awardId ");
		hqlQuery.append(" where T3.awardId = : awardId and T1.positionId is not null and (T1.personId is null or T1.personId <> '999999999100') and T2.manpowerTypeCode = '1' group by T1.positionId, T1.personId");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("awardId", awardId);
		return query.getResultList();
	}

	@Override
	public List<AwardManpower> fetchAwardManpowerDetailsByIOCode(Set<String> budgetReferenceNumbers, Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpower> query = builder.createQuery(AwardManpower.class);
		Root<AwardManpower> root = query.from(AwardManpower.class);
		Predicate predicateAwardId = builder.equal(root.get("awardId"), awardId);
		Predicate predicateBudgetRef = root.get("budgetReferenceNumber").in(budgetReferenceNumbers);
		if(!budgetReferenceNumbers.isEmpty()) {
			query.where(builder.and(predicateAwardId, predicateBudgetRef));
		} else {
			query.where(builder.and(predicateAwardId));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardManpowerResource> getAllAwardManpowerResourcesByManpowerIds(Set<Integer> awardManpowerIds) {
		if(!awardManpowerIds.isEmpty()) {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
			Root<AwardManpowerResource> root = query.from(AwardManpowerResource.class);
			query.where(root.get("awardManpowerId").in(awardManpowerIds));
			return session.createQuery(query).getResultList();
		}
		return new ArrayList<>();
	}

	@Override
	public List<AwardManpowerResource> getAwardManpowerResourcesByManpowerId(Integer awardManpowerId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
		Root<AwardManpowerResource> rootAwardManpowerResource = query.from(AwardManpowerResource.class);
		Predicate predicateAwardManpowerId = builder.equal(rootAwardManpowerResource.get("awardManpowerId"), awardManpowerId);
		query.where(builder.and(predicateAwardManpowerId));
		return session.createQuery(query).getResultList();
	}

}
