package com.polus.fibicomp.manpowerintegration.dao;

import java.math.BigDecimal;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.DateTimeService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.manpower.pojo.AwardManpower;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.manpower.pojo.ManpowerInterfaceStatus;
import com.polus.fibicomp.manpower.pojo.ManpowerJobProfileType;
import com.polus.fibicomp.manpower.pojo.ManpowerTemp;
import com.polus.fibicomp.manpower.pojo.ManpowerUserAction;
import com.polus.fibicomp.manpower.pojo.WorkdayManpowerInterface;
import com.polus.fibicomp.manpowerintegration.dto.AwardClosePositionResorceDto;
import com.polus.fibicomp.manpowerintegration.dto.WorkdayInterfaceLogDto;
import com.polus.fibicomp.manpowerintegration.dto.terminations.TerminationsResourceResultDto;
import com.polus.fibicomp.manpowerintegration.pojo.AwardManpowerBaseSalaryHistory;
import com.polus.fibicomp.manpowerintegration.pojo.AwardSupOrgMapping;
import com.polus.fibicomp.manpowerintegration.pojo.ManpowerLog;
import com.polus.fibicomp.manpowerintegration.pojo.MigrationManpowerPerson;
import com.polus.fibicomp.manpowerintegration.pojo.RiseErrorAllocations;
import com.polus.fibicomp.manpowerintegration.pojo.WorkdayConfigurationData;
import com.polus.fibicomp.manpowerintegration.pojo.WorkdayJobProfileChange;
import com.polus.fibicomp.manpowerintegration.pojo.WorkdayLongLeaveDetails;
import com.polus.fibicomp.manpowerintegration.pojo.WorkdayTerminationDetails;
import com.polus.fibicomp.manpowerintegration.vo.AwardPersonVo;
import com.polus.fibicomp.manpowerintegration.vo.ManpowerIntegrationVO;

import oracle.jdbc.internal.OracleTypes;

@Transactional
@Service(value = "manpowerIntegrationDao")
public class ManpowerIntegrationDaoImpl implements ManpowerIntegrationDao {

	protected static Logger logger = LogManager.getLogger(ManpowerIntegrationDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private DateTimeService dateTimeService;

	@Autowired
	private CommonDao commonDao; 

	@Autowired
	private EntityManager entityManager;

	@Value("${oracledb}")
	private String oracledb;

	@Override
	public List<WorkdayManpowerInterface> fetchManpowerInterfacesByInterfaceStatus(String interfaceStatus) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<WorkdayManpowerInterface> query = builder.createQuery(WorkdayManpowerInterface.class);
		Root<WorkdayManpowerInterface> manpowerInterface= query.from(WorkdayManpowerInterface.class);
		Predicate predicatePersonId= builder.equal(manpowerInterface.get("interfaceStatusCode"), interfaceStatus);
		query.where(builder.and(predicatePersonId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Award> getActiveAwardsByAwardPersonAndRoles(String personId, List<Integer> roleIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> query = builder.createQuery(Award.class);
		Root<Award> award= query.from(Award.class);
		Predicate predicateSequenceStatus= builder.equal(award.get("awardSequenceStatus"), "ACTIVE");
		Join<Award, AwardPerson> join = award.join("awardPersons");
		Predicate predicatePersonId= builder.equal(join.get("personId"), personId);
		Predicate predicatePersonRoles = (builder.in(join.get("personRoleId")).value(roleIds));
		query.where(builder.and(predicateSequenceStatus, predicatePersonId, predicatePersonRoles));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardManpowerResource> getAwardManpowerResourcesByPersonIdAndPositionStatus(String personId, String positionStatusCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
		Root<AwardManpowerResource> rootAwardManpowerResource = query.from(AwardManpowerResource.class);
		Predicate predicatePersonId = builder.equal(rootAwardManpowerResource.get("personId"), personId);
		Predicate predicatePositionStatusCode = builder.equal(rootAwardManpowerResource.get("positionStatusCode"), positionStatusCode);
		query.where(builder.and(predicatePersonId, predicatePositionStatusCode));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String getManpowerConfigurationValue(String keyName) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "select M.configurationValue from ManpowerConfigurationData M WHERE M.configurationKey = :keyName";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("keyName", keyName);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			return "";
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<TerminationsResourceResultDto> getTerminatedResourcesByPersonIdAndPositionStatusAndTerminationDate(String personId, String positionStatusCode, Timestamp terminationDate) {
		String hqlQuery = "select NEW com.polus.fibicomp.manpowerintegration.dto.terminations.TerminationsResourceResultDto(t2.awardId, t1.fullName, t1.personId) from AwardManpowerResource t1 inner join AwardManpower t2 on t2.awardManpowerId = t1.awardManpowerId inner join Award t3 on t2.awardId = t3.awardId where t1.personId =:personId and t1.positionStatusCode =:positionStatusCode and t3.awardSequenceStatus ='ACTIVE' ";
		if (terminationDate != null) {
			hqlQuery = hqlQuery  + "and t1.chargeEndDate >:terminationDate";
		}
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery);
		query.setParameter("personId",personId);
		query.setParameter("positionStatusCode",positionStatusCode);
		if (terminationDate != null) {
			query.setParameter("terminationDate", terminationDate);
		}
		return query.getResultList();
	}

	@Override
	public String getPIPersonIdByAwardId(Integer awardId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "select P.personId from AwardPerson P WHERE P.awardId = :awardId and P.isPi = 'Y' ";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("awardId", awardId);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			return "";
		}
	}

	@Override
	public String getReferSubUnitFlagByUnitNumber(String unitNumber) {
		try {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createSQLQuery("SELECT REFER_SUB_UNIT_FLAG FROM SAP_FEED_UNIT_MAPPING WHERE UNIT_NUMBER = :unitNumber");
		query.setParameter("unitNumber", unitNumber);
		return query.getSingleResult().toString();
		} catch (Exception e) {
			return "";
		}
	}

	@Override
	public String getSuperiorSupervisoryOrganizationIdByUnitNumber(String unitNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query query = session.createSQLQuery("SELECT SUPERIOR_SUP_ORG FROM SAP_FEED_UNIT_MAPPING WHERE UNIT_NUMBER = :unitNumber");
			query.setParameter("unitNumber", unitNumber);
			return query.getSingleResult().toString();
		} catch (Exception e) {
			return "";
		}
	}

	@Override
	public List<WorkdayManpowerInterface> fetchManpowerInterfacesForCostAllocationAfterManpower(String interfaceStatus, String interfaceType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<WorkdayManpowerInterface> query = builder.createQuery(WorkdayManpowerInterface.class);
		Root<WorkdayManpowerInterface> manpowerInterface= query.from(WorkdayManpowerInterface.class);
		Predicate predicateInterfaceStatus= builder.equal(manpowerInterface.get("interfaceStatusCode"), interfaceStatus);
		Predicate predicateInterfaceType= builder.equal(manpowerInterface.get("interfaceTypeCode"), interfaceType);
		Predicate predicatePersonId = manpowerInterface.get("awardManpowerResource").get("personId").isNotNull();
		Predicate predicateFullName = manpowerInterface.get("awardManpowerResource").get("fullName").isNotNull();
		Predicate predicateChargeStartDate = manpowerInterface.get("awardManpowerResource").get("chargeStartDate").isNotNull();
		Predicate predicateChargeEndDate = manpowerInterface.get("awardManpowerResource").get("chargeEndDate").isNotNull();
		query.where(builder.and(predicateInterfaceStatus, predicateInterfaceType, predicatePersonId, predicateChargeStartDate, predicateChargeEndDate, predicateFullName));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardManpowerResource getAwardManpowerResourceById(Integer awardManpowerResourceId) {
		return hibernateTemplate.get(AwardManpowerResource.class, awardManpowerResourceId);
	}

	@Override
	public WorkdayManpowerInterface saveOrUpdateManpowerInterface(WorkdayManpowerInterface manpowerInterface) {
		try {
			hibernateTemplate.saveOrUpdate(manpowerInterface);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateManpowerInterface {}", e.getMessage());
		}
		return manpowerInterface;
	}

	@Override
	public void saveOrUpdateJobProfileType(ManpowerJobProfileType jobProfile) {
		try {
			hibernateTemplate.saveOrUpdate(jobProfile);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateJobProfileType: {} ", e.getMessage());
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean checkIfAwardHasManpowerStaffResource(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT t1 from AwardManpower t1 inner join AwardManpowerResource t2 on t1.awardManpowerId = t2.awardManpowerId where t1.awardId =:awardId and t1.manpowerTypeCode = '1' and (t2.personId is null or t2.personId <> '999999999100')";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("awardId", awardId);
		List<AwardManpower> awardManpower = query.getResultList();
		if (awardManpower != null && !awardManpower.isEmpty()) {
			return true;
		} else {
			return false;
		}
	}

	@Override
	public void saveOrUpdateManpowerLog(ManpowerLog manpowerLog) {
		Session session = null;
		try {
			session = hibernateTemplate.getSessionFactory().openSession();
			Transaction trans = session.beginTransaction();
			session.saveOrUpdate(manpowerLog);
			trans.commit();
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateManpowerLog : {} ", e.getMessage());
		}finally {
			if(session != null)
				session.close();
		}
	}

	@Override
	public String getPiSupOrgByPiPersonIdAndSuperior(String pIPersonId, String superiorSupOrgId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "select P.supOrgId from AwardSupOrgMapping P  WHERE P.pIPersonId = :pIPersonId and P.superiorSupOrgId = :superiorSupOrgId";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("pIPersonId", pIPersonId);
			query.setParameter("superiorSupOrgId", superiorSupOrgId);
			return (String) query.getResultList().get(0);
		} catch (Exception e) {
			return "";
		}
	}

	@Override
	public void saveOrUpdateAwardSupOrgMapping(AwardSupOrgMapping awardSupOrgMapping) {
		Session session = null;
		try {
			session = hibernateTemplate.getSessionFactory().openSession();
			Transaction trans = session.beginTransaction();
			session.saveOrUpdate(awardSupOrgMapping);
			trans.commit();			
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardSupOrgMapping : {} ", e.getMessage());
		}finally {
			if(session != null)
				session.close();
		}
	}

	@Override
	public List<WorkdayManpowerInterface> fetchManpowerInterfacesByAwardNumberAndInterfaceStatusAndInterfaceTypeCodesAndResourceUniqueId(String awardNumber, String manpowerInterfaceStatus, List<String> manpowerInterfaceTypes, String resourceUniqueId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<WorkdayManpowerInterface> query = builder.createQuery(WorkdayManpowerInterface.class);
		Root<WorkdayManpowerInterface> rootManpowerInterface= query.from(WorkdayManpowerInterface.class);
		Predicate predicateInterfaceStatus= builder.equal(rootManpowerInterface.get("interfaceStatusCode"), manpowerInterfaceStatus);
		Predicate predicateAwardNumber = builder.equal(rootManpowerInterface.get("awardNumber"), awardNumber);
		if ((manpowerInterfaceTypes == null || manpowerInterfaceTypes.isEmpty()) && resourceUniqueId == null) {
			query.where(builder.and(predicateInterfaceStatus, predicateAwardNumber));
		} else if ((manpowerInterfaceTypes != null && !manpowerInterfaceTypes.isEmpty()) && resourceUniqueId != null) {
			Predicate predicateInterfaceType= rootManpowerInterface.get("interfaceTypeCode").in(manpowerInterfaceTypes);
			Predicate predicateResourceUniqueId = builder.equal(rootManpowerInterface.get("resourceUniqueId"), resourceUniqueId);
			query.where(builder.and(predicateInterfaceStatus, predicateAwardNumber, predicateInterfaceType, predicateResourceUniqueId));
		} else {
			Predicate predicateInterfaceType= rootManpowerInterface.get("interfaceTypeCode").in(manpowerInterfaceTypes);
			query.where(builder.and(predicateInterfaceStatus, predicateAwardNumber, predicateInterfaceType));
		}
		query.orderBy(builder.asc(rootManpowerInterface.get("workdayManpowerInterfaceId")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardSupOrgMapping getLatestAwardSupOrgByAwardNumber(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM AwardSupOrgMapping WHERE awardSupOrgMappingId = (select max(awardSupOrgMappingId) from AwardSupOrgMapping where awardNumber =:awardNumber)";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("awardNumber", awardNumber);
		if (query.getResultList() != null && !(query.getResultList()).isEmpty()) {
			return (AwardSupOrgMapping) query.getSingleResult();
		}
		return null;
	}

	@Override
	public void saveOrUpdateAwardManpowerResource(AwardManpowerResource awardManpowerResource) {
		try {
			hibernateTemplate.saveOrUpdate(awardManpowerResource);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardManpowerResource : {} ", e.getMessage());
		}
	}

	@Override
	public List<AwardManpowerResource> getAwardManpowerResourcesByResourceUniqueId(String resourceUniqueId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
		Root<AwardManpowerResource> manpowerInterface= query.from(AwardManpowerResource.class);
		Predicate predicateResourceUniqueId= builder.equal(manpowerInterface.get("resourceUniqueId"), resourceUniqueId);
		query.where(builder.and(predicateResourceUniqueId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void saveOrUpdateWorkdayManpowerInterface(WorkdayManpowerInterface workdayManpowerInterface) {
		try {
			hibernateTemplate.saveOrUpdate(workdayManpowerInterface);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateWorkdayManpowerInterface : {} ", e.getMessage());
		}
	}

	@Override
	public boolean checkIfAwardPISUpOrgMappingExist(String awardNumber, String pIPersonId, String superiorSupOrgId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardSupOrgMapping> query = builder.createQuery(AwardSupOrgMapping.class);
		Root<AwardSupOrgMapping> rootAwardPISupOrgMapping = query.from(AwardSupOrgMapping.class);
		Predicate predicateOne = builder.equal(rootAwardPISupOrgMapping.get("awardNumber"), awardNumber);
		Predicate predicateTwo = builder.equal(rootAwardPISupOrgMapping.get("pIPersonId"), pIPersonId);
		Predicate predicateThree = builder.equal(rootAwardPISupOrgMapping.get("superiorSupOrgId"), superiorSupOrgId);
		query.where(builder.and(predicateOne, predicateTwo, predicateThree));
		List<AwardSupOrgMapping> awardPISupOrgMappings = session.createQuery(query).getResultList();
		if (awardPISupOrgMappings != null && !awardPISupOrgMappings.isEmpty()) {
			return true;
		} else {
			return false;
		}
	}

	@Override
	public AwardManpower getAwardManpowerById(Integer awardManpowerId) {
		return hibernateTemplate.get(AwardManpower.class, awardManpowerId);
	}

	@Override
	public List<AwardSupOrgMapping> getAwardUpOrgMappingByAwardNumberAndPIPersonId(String awardNumber, String pIPersonId, String superiorSupOrgId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardSupOrgMapping> query = builder.createQuery(AwardSupOrgMapping.class);
		Root<AwardSupOrgMapping> rootawardSupOrgMapping= query.from(AwardSupOrgMapping.class);
		Predicate predicateAwardNumber= builder.equal(rootawardSupOrgMapping.get("awardNumber"), awardNumber);
		Predicate predicatePIPersonId = builder.equal(rootawardSupOrgMapping.get("pIPersonId"), pIPersonId);
		Predicate predicateSuperiorSupOrgId = builder.equal(rootawardSupOrgMapping.get("superiorSupOrgId"), superiorSupOrgId);
		query.where(builder.and(predicateAwardNumber, predicatePIPersonId, predicateSuperiorSupOrgId));
		query.orderBy(builder.asc(rootawardSupOrgMapping.get("awardSupOrgMappingId")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardManpowerResource> getOwnedAwardManpowerResourcesByAwardNumber(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
		Root<AwardManpowerResource> rootAwardManpowerResource = query.from(AwardManpowerResource.class);
		Predicate predicateAwardNumber = builder.equal(rootAwardManpowerResource.get("awardNumber"), awardNumber);
		Predicate predicatePIPersonId = rootAwardManpowerResource.get("positionId").isNotNull();
		Predicate predicatePosition = builder.equal(rootAwardManpowerResource.get("positionOwnedByAward"), "Y");
		Predicate predicatePersonId = rootAwardManpowerResource.get("personId").isNull();
		Predicate predicateExpiredDummyPerson = builder.notEqual(rootAwardManpowerResource.get("personId"), "999999999100");
		Predicate predicatePerson = builder.or(predicatePersonId, predicateExpiredDummyPerson);
		query.where(builder.and(predicateAwardNumber, predicatePIPersonId, predicatePosition, predicatePerson));
		query.multiselect(rootAwardManpowerResource.get("personId"), rootAwardManpowerResource.get("positionId"),
				rootAwardManpowerResource.get("awardNumber")).distinct(true);
		return session.createQuery(query).getResultList();
	}

	@Override
	public Manpower getManpowerByPersonId(String personId) {
		try {
			return hibernateTemplate.get(Manpower.class, personId);
		} catch (Exception e) {
			logger.error("error occured in getManpowerByPersonId : {}", e.getMessage());
			return null;
		}
	}

	@Override
	public void saveOrUpdate(Manpower manpower) {
		hibernateTemplate.saveOrUpdate(manpower);
	}

	@Override
	public List<AwardManpowerResource> getManpowerResourceByPostionIdAndStatuses(String positionId, List<String> statuses) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		List<String> statusCodes = new ArrayList<>();
		statusCodes.addAll(statuses);
		CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
		Root<AwardManpowerResource> rootAwardManpowerResource = query.from(AwardManpowerResource.class);
		Predicate predicatePostion = builder.equal(rootAwardManpowerResource.get("positionId"), positionId);
		Predicate predicateStatus = rootAwardManpowerResource.get("positionStatusCode").in(statusCodes);
		query.where(builder.and(predicatePostion, predicateStatus));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<String> fetchAwardNumberByActive(Timestamp yesterday) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> query = builder.createQuery(String.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicateActive = builder.equal(rootAward.get("awardSequenceStatus"), Constants.AWARD_FINAL_STATUS_ACTIVE);
		Predicate predicateEndDate = builder.equal(rootAward.get("finalExpirationDate"), yesterday);
		query.where(builder.and(predicateActive, predicateEndDate));
		query.select(rootAward.get("awardNumber"));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardManpowerResource> getPostionIdsByAwardNumber(List<String> awardNumbers) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
		Root<AwardManpowerResource> rootAwardManpowerResource = query.from(AwardManpowerResource.class);
		Predicate predicatePostion = builder.equal(rootAwardManpowerResource.get("positionOwnedByAward"), Constants.YES);
		Predicate predicatePositionNotNull = rootAwardManpowerResource.get("positionId").isNotNull();
		Predicate predicateAwardNumber = rootAwardManpowerResource.get("awardNumber").in(awardNumbers);
		Predicate predicatePersonId = rootAwardManpowerResource.get("personId").isNull();
		Predicate predicateExpiredDummyPerson = builder.notEqual(rootAwardManpowerResource.get("personId"), "999999999100");
		Predicate predicatePerson = builder.or(predicatePersonId, predicateExpiredDummyPerson);
		query.where(builder.and(predicatePostion, predicateAwardNumber, predicatePositionNotNull, predicatePerson));
		query.multiselect(rootAwardManpowerResource.get("awardNumber"), rootAwardManpowerResource.get("positionId")).distinct(true);
		return session.createQuery(query).getResultList();
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<AwardManpowerResource> getManpoerResourceByActualAmountNull() {
		List<String> statuses = new ArrayList<>();
		statuses.add(Constants.MANPOWER_POSITION_GENERATED);
		statuses.add(Constants.MANPOWER_PENDING_APPROVAL);
		statuses.add(Constants.MANPOWER_ENDED);
		statuses.add(Constants.MANPOWER_HIRING_ON_EXISTING_POSITION);
		statuses.add(Constants.MANPOWER_EXPIRED);		
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select T3 From Award T1 inner join AwardManpower T2 on T2.awardId = T1.awardId");
		hqlQuery.append(" inner join AwardManpowerResource T3 on T3.awardManpowerId = T2.awardManpowerId");
		hqlQuery.append("  where T1.awardSequenceStatus in ('ACTIVE') and T3.positionStatusCode in :statuses and T3.committedCost is null and T3.personId is not null and T3.fullName is not null ");
		Query findManpowerResources = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		findManpowerResources.setParameter("statuses", statuses);
		return findManpowerResources.getResultList();
	}

	@Override
	public List<Manpower> getAllManPowerDetails() {
		return hibernateTemplate.loadAll(Manpower.class);
	}

	@Override
	public void saveManpowerTemp(ManpowerTemp manpowerTemp) {
		Session session = null;
		try {
			session = hibernateTemplate.getSessionFactory().openSession();
			Transaction trans = session.beginTransaction();
			session.saveOrUpdate(manpowerTemp);
			trans.commit();
		} catch (Exception e) {
			logger.error("Exception in saveManpowerTemp : {} ", e.getMessage());
		}finally {
			if(session != null)
				session.close();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<WorkdayConfigurationData> getWorkdayConfigurationData() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM WorkdayConfigurationData W where configurationKey in ('SUBSCRIPTION_KEY_VALUE','WORKDAY_API')";
		Query query = session.createQuery(hqlQuery);
		return query.getResultList();
	}

	@Override
	public void saveOrUpdateLongLeaveData(WorkdayLongLeaveDetails longLeaveData) {
		hibernateTemplate.saveOrUpdate(longLeaveData);

	}

	@Override
	public void saveOrUpdateWorkdayTerminations(WorkdayTerminationDetails terminationDetails) {
		hibernateTemplate.saveOrUpdate(terminationDetails);

	}

	@Override
	public void saveOrUpdateJobProfileChanges(WorkdayJobProfileChange workdayJobProfile) {
		hibernateTemplate.saveOrUpdate(workdayJobProfile);

	}

	@SuppressWarnings("unchecked")
	@Override
	public List<WorkdayTerminationDetails> getTerminationDetailsByPersonId(String personId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "FROM WorkdayTerminationDetails WHERE personId = :personId and triggerDate >:triggerDate";			
			Query query = session.createQuery(hqlQuery);
			query.setParameter("triggerDate", dateTimeService.getCurrentDateWithZeroTime(commonDao.getCurrentTimestamp()));
			query.setParameter("personId", personId);
			return query.getResultList();
		} catch (Exception e) {
			logger.error("error occured in getTerminationDetailsByPersonId : {}", e.getMessage());
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<WorkdayLongLeaveDetails> getLongLeaveDetailsByPersonId(String personId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "FROM WorkdayLongLeaveDetails WHERE personId = :personId and triggerDate >:triggerDate";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("triggerDate", dateTimeService.getCurrentDateWithZeroTime(commonDao.getCurrentTimestamp()));
			query.setParameter("personId", personId);
			return query.getResultList();
		} catch (Exception e) {
			logger.error("error occured in getLongLeaveDetailsByPersonId : {}", e.getMessage());
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public WorkdayJobProfileChange getJobProfileDetailsByPersonId(String personId) {
		try {
			List<WorkdayJobProfileChange> jobProfileChanges = null;
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "FROM WorkdayJobProfileChange WHERE personId = :personId";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("personId", personId);
			jobProfileChanges =  query.getResultList();
			if (jobProfileChanges != null && !jobProfileChanges.isEmpty()) {
				return jobProfileChanges.get(0);
			}
		} catch (Exception e) {
			logger.error("error occured in getJobProfileDetailsByPersonId : {}", e.getMessage());
			return null;
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardPersonVo> getActiveAwardsAndLeadUnitNumberByParams(List<String> personIds, Timestamp currentDate) {
		String hqlQuery = "select new com.polus.fibicomp.manpowerintegration.vo.AwardPersonVo(t1.awardId, t2.personId, t2.fullName, t1.leadUnitNumber,t1.awardNumber, t3.unitName) "
				+ "from Award t1 inner join AwardPerson t2 on t1.awardId=t2.awardId inner join Unit t3 on t1.leadUnitNumber = t3.unitNumber where "
				+ "t1.awardSequenceStatus = 'ACTIVE' and t2.personId in(:personIds) and t1.finalExpirationDate >= :currentDate and t2.personRoleId in (3,9)";
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery);
		query.setParameter("personIds", personIds);
		query.setParameter("currentDate", currentDate);
		return query.getResultList();
	}

	@Override
	public void deleteOldRowsBasedTerminationTriggerDate(Timestamp deleteFromDate) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "delete WorkdayTerminationDetails where triggerDate < :deleteFromDate";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("deleteFromDate", deleteFromDate);
			query.executeUpdate();			
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("error occured in deleteOldRowsBasedTriggerDate : {}", e.getMessage());
		}
		return;
	}

	@Override
	public void deleteOldRowsBasedLongLeaveTriggerDate(Timestamp deleteFromDate) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "delete WorkdayLongLeaveDetails where triggerDate < :deleteFromDate";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("deleteFromDate", deleteFromDate);
			query.executeUpdate();
		} catch (Exception e) {
			logger.error("error occured in deleteOldRowsBasedLongLeaveTriggerDate : {}", e.getMessage());
		}
		return;		
	}

	@SuppressWarnings("unchecked")
	@Override
	public WorkdayLongLeaveDetails getLongLeaveDataByPersonIdAndUniqueInitiated(String personId, String initiated) {
		try {
			List<WorkdayLongLeaveDetails> workdayLongLeaveDetails = null;
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "FROM WorkdayLongLeaveDetails WHERE personId = :personId and uniqueInitiated = :uniqueInitiated";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("personId", personId);
			query.setParameter("uniqueInitiated", initiated);
			workdayLongLeaveDetails = query.getResultList();
			if(workdayLongLeaveDetails != null && !workdayLongLeaveDetails.isEmpty()) {
				return workdayLongLeaveDetails.get(0);
			}
		} catch (Exception e) {
			logger.error("error occured in getLongLeaveDataByPersonIdAndUniqueInitiated : {}", e.getMessage());
			return null;
		}
		return null;
	}

	@Override
	public List<ManpowerLog> getManpowerLogDetails(String messageType) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ManpowerLog> criteria = builder.createQuery(ManpowerLog.class);
			Root<ManpowerLog> protocolRoot = criteria.from(ManpowerLog.class);
			Predicate predicateMessageType = builder.equal(protocolRoot.get("messageType"), messageType);
			Predicate predicateTimeStampMidnight = builder.greaterThan(protocolRoot.get("updateTimeStamp"), dateTimeService.getCurrentDateWithZeroTime(commonDao.getCurrentTimestamp()));
			criteria.where(builder.and(predicateMessageType, predicateTimeStampMidnight));
			return session.createQuery(criteria).getResultList();
		});
	}
	
	@Override
	public String getPIFullNameByAwardId(Integer awardId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "select P.fullName from AwardPerson P WHERE P.awardId = :awardId and P.isPi = 'Y' ";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("awardId", awardId);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			return "";
		}
	}

	@Override
	public List<AwardManpowerResource> getManpowerResourcesbyPersonIdAndWithoutName(String manpowerPersonId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
		Root<AwardManpowerResource> rootAwardManpowerResource = query.from(AwardManpowerResource.class);
		Predicate predicateCommitedCost = rootAwardManpowerResource.get("fullName").isNull();
		Predicate predicatePersonId = builder.equal(rootAwardManpowerResource.get("personId"), manpowerPersonId);
		query.where(builder.and(predicateCommitedCost, predicatePersonId));
		return session.createQuery(query).getResultList();
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public WorkdayTerminationDetails getTerminationDataByEmployeeIdAndEventInitiation(String personId, String eventInitiationDate) {
		List<WorkdayTerminationDetails> workdayTerminationDetails = null;
		try {			
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "FROM WorkdayTerminationDetails WHERE personId = :personId and uniqueEventInitiationDate = :uniqueEventInitiationDate";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("personId", personId);
			query.setParameter("uniqueEventInitiationDate", eventInitiationDate);
			workdayTerminationDetails = query.getResultList();
			if(workdayTerminationDetails != null && !workdayTerminationDetails.isEmpty()) {
				return workdayTerminationDetails.get(0);
			}
		} catch (Exception e) {
			logger.error("error occured in getTerminationDataByEmployeeIdAndEventInitiation : {}", e.getMessage());
			return null;
		}
		return null;
	}

	@Override
	public List<AwardManpowerResource> getManpowerResourcesByParams(String positionId, List<String> statuses) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		List<String> statusCodes = new ArrayList<>();
		statusCodes.addAll(statuses);
		CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
		Root<AwardManpowerResource> rootAwardManpowerResource = query.from(AwardManpowerResource.class);
		Predicate predicatePostion = builder.equal(rootAwardManpowerResource.get("positionId"), positionId);
		Predicate predicateStatus = rootAwardManpowerResource.get("positionStatusCode").in(statusCodes);
		Predicate predicateChargeStartDate = rootAwardManpowerResource.get("chargeStartDate").isNull();
		Predicate predicateChargeEndDate = rootAwardManpowerResource.get("chargeEndDate").isNull();
		query.where(builder.and(predicatePostion, predicateStatus, predicateChargeStartDate, predicateChargeEndDate));
		return session.createQuery(query).getResultList();
	}

	@Override
	public WorkdayManpowerInterface getLatestResourceInterfaceByParams(String resourceUniqueId, List<String> interfaceTypes) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM WorkdayManpowerInterface WHERE workdayManpowerInterfaceId = (select max(workdayManpowerInterfaceId) from WorkdayManpowerInterface where resourceUniqueId =:resourceUniqueId and interfaceTypeCode in :interfaceTypes)";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("resourceUniqueId", resourceUniqueId);
		query.setParameter("interfaceTypes", interfaceTypes);
		if (query.getResultList() != null && !(query.getResultList()).isEmpty()) {
			return (WorkdayManpowerInterface) query.getSingleResult();
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardManpowerResource> getAwardManpowerResourcesResourceUniqueIdAndAwardSequenceStatues(String resourceUniqueId, List<String> awardStatuses) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select T3 From Award T1 inner join AwardManpower T2 on T2.awardId = T1.awardId");
		hqlQuery.append(" inner join AwardManpowerResource T3 on T3.awardManpowerId = T2.awardManpowerId");
		hqlQuery.append("  where T1.awardSequenceStatus in :awardStatuses and T3.resourceUniqueId = : resourceUniqueId ");
		Query findManpowerResources = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		findManpowerResources.setParameter("resourceUniqueId", resourceUniqueId);
		findManpowerResources.setParameter("awardStatuses", awardStatuses);
		return findManpowerResources.getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardManpowerResource> getAwardManpowerResourcesByAwardId(Integer awardId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select T3 from AwardManpower T2 ");
		hqlQuery.append(" inner join AwardManpowerResource T3 on T3.awardManpowerId = T2.awardManpowerId");
		hqlQuery.append("  where T3.personId is not null and T2.awardId = : awardId and T2.manpowerTypeCode = : type and T3.personId <> '999999999100'");
		Query manpowerResources = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		manpowerResources.setParameter("awardId", awardId);
		manpowerResources.setParameter("type", Constants.MANPOWER_TYPE_STAFF);
		return manpowerResources.getResultList();
	}

	@Override
	public List<MigrationManpowerPerson> getallMigratedManpowerPersons() {
		return hibernateTemplate.loadAll(MigrationManpowerPerson.class);
	}

	@Override
	public Integer getCountOfPositions(String positionId) {
		try {
			Integer count = 0;
			Query query = entityManager.createNativeQuery("SELECT COUNT(distinct POSITION_ID, AWARD_NUMBER) FROM AWARD_MANPOWER_RESOURCE WHERE POSITION_ID = :positionId and POSITION_OWNED_BY_AWARD ='Y'");
			query.setParameter("positionId", positionId);
			count = ((Number) query.getSingleResult()).intValue();
			return count;
		} catch (Exception e) {
			logger.error("error in getCountOfPositions {}", e.getMessage());
			return null;
		}
	}

	@Override
	public AwardManpowerBaseSalaryHistory getPreviousBaseSalaryFromHistory(String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM AwardManpowerBaseSalaryHistory WHERE baseSalaryHistoryId = (select max(baseSalaryHistoryId) from AwardManpowerBaseSalaryHistory where personId =:personId)";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("personId", personId);
		if (query.getResultList() != null && !(query.getResultList()).isEmpty()) {
			return (AwardManpowerBaseSalaryHistory) query.getSingleResult();
		}
		return null;
	}

	@Override
	public void saveOrUpdateAwardManpowerBaseSalaryHistory(AwardManpowerBaseSalaryHistory baseSalaryHistory) {
		try {
			hibernateTemplate.saveOrUpdate(baseSalaryHistory);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardManpowerBaseSalaryHistory {}", e.getMessage());
		}
	}

	@Override
	public BigDecimal getBudgetAmountByAwardIdAndBudgetReferenceNumber(Integer awardId, String budgetReferenceNumber) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select T2.lineItemCost From AwardBudgetHeader T1 inner join AwardBudgetDetail T2 on T2.budgetId = T1.budgetId");
		hqlQuery.append("  where T1.awardId = : awardId  and T2.internalOrderCode = : budgetReferenceNumber ");
		Query budgetAmount = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		budgetAmount.setParameter("awardId", awardId);
		budgetAmount.setParameter("budgetReferenceNumber", budgetReferenceNumber);
		try {
			return (BigDecimal) budgetAmount.getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public AwardSupOrgMapping getLatestAwardSupOrgByPI(String awardNumber, String pIPersonId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM AwardSupOrgMapping WHERE awardSupOrgMappingId = (select max(awardSupOrgMappingId) from AwardSupOrgMapping where pIPersonId =:pIPersonId and awardNumber =:awardNumber)";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("pIPersonId", pIPersonId);
		query.setParameter("awardNumber", awardNumber);
		if (query.getResultList() != null && !(query.getResultList()).isEmpty()) {
			return (AwardSupOrgMapping) query.getSingleResult();
		}
		return null;
	}

	@Override
	public List<AwardClosePositionResorceDto> getClosePositionAwardPositionsList() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		List<AwardClosePositionResorceDto> resources = new ArrayList<>();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_MANPOWER_CLOSE_POSITION_LIST()}");
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_MANPOWER_CLOSE_POSITION_LIST";
				String functionCall = "{call " + procedureName + "(?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
			while (rset.next()) {
				AwardClosePositionResorceDto resource = new AwardClosePositionResorceDto((Integer) rset.getInt("AWARD_ID"), 
						(String) rset.getString("AWARD_NUMBER"), (String) rset.getString("POSITION_ID"), (String) rset.getString("EXCLUSION_TYPE"), 
						(String) rset.getString("SCHOOL"));
				resources.add(resource);
			}
		} catch (SQLException e) {
			logger.error("Exception in getClosePositionAwardPositions: {} ", e.getMessage());
		}
		return resources;
	}

	@Override
	public WorkdayManpowerInterface getWorkdayManpowerInterfaceById(Integer workdayManpowerInterfaceId) {
		return hibernateTemplate.get(WorkdayManpowerInterface.class, workdayManpowerInterfaceId);
	}

	@Override
	public ManpowerLog getManpowerLogById(Integer manpowerLogId) {
		return hibernateTemplate.get(ManpowerLog.class, manpowerLogId);
	}

	@Override
	public Timestamp getLastSyncedOnTime(String manpowerInterfaceTypeCode, String manpowerInterfaceSuccess) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "Select updateTimeStamp FROM ManpowerLog WHERE manpowerLogId = (select max(manpowerLogId) from ManpowerLog where interfaceTypeCode =:manpowerInterfaceTypeCode and messageType =:manpowerInterfaceSuccess)";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("manpowerInterfaceTypeCode", manpowerInterfaceTypeCode);
		query.setParameter("manpowerInterfaceSuccess", manpowerInterfaceSuccess);
		try {
			return (Timestamp) query.getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public Integer getNoOfRecords(String manpowerInterfaceTypeCode, Timestamp startDate, Timestamp endDate) {
		Integer count = 0;
		try {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		if (manpowerInterfaceTypeCode.equals(Constants.MANPOWER_INTERFACE_TERMINATION_AND_RESIGNATION)) {
			CriteriaQuery<WorkdayTerminationDetails> query = builder.createQuery(WorkdayTerminationDetails.class);
			Root<WorkdayTerminationDetails> root = query.from(WorkdayTerminationDetails.class);
			query.where(builder.and(builder.between(root.get("updateTimestamp"), startDate, endDate)));
			count =  session.createQuery(query).getResultList().size();
		} else if (manpowerInterfaceTypeCode.equals(Constants.MANPOWER_INTERFACE_LONG_LEAVE)) {
			CriteriaQuery<WorkdayLongLeaveDetails> query = builder.createQuery(WorkdayLongLeaveDetails.class);
			Root<WorkdayLongLeaveDetails> root = query.from(WorkdayLongLeaveDetails.class);
			query.where(builder.and(builder.between(root.get("updateTimestamp"), startDate, endDate)));
			count =  session.createQuery(query).getResultList().size();
		} else if (manpowerInterfaceTypeCode.equals(Constants.MANPOWER_INTERFACE_DESIGNATION_CHANGE)) {
			CriteriaQuery<WorkdayJobProfileChange> query = builder.createQuery(WorkdayJobProfileChange.class);
			Root<WorkdayJobProfileChange> root = query.from(WorkdayJobProfileChange.class);
			query.where(builder.and(builder.between(root.get("updateTimestamp"), startDate, endDate)));
			count =  session.createQuery(query).getResultList().size();
		} else if (manpowerInterfaceTypeCode.equals(Constants.MANPOWER_INTERFACE_MANPOWER_DETAILS)) {
			CriteriaQuery<Manpower> query = builder.createQuery(Manpower.class);
			Root<Manpower> root = query.from(Manpower.class);
			query.where(builder.and(builder.between(root.get("updateTimestamp"), startDate, endDate)));
			count =  session.createQuery(query).getResultList().size();
		} else if (manpowerInterfaceTypeCode.equals(Constants.MANPOWER_INTERFACE_CITIZENSHIP_NATIONALITY)) {
			CriteriaQuery<Manpower> query = builder.createQuery(Manpower.class);
			Root<Manpower> root = query.from(Manpower.class);
			Predicate date = builder.between(root.get("updateTimestamp"), startDate, endDate);
			Predicate citizenship = root.get("citizenship").isNotNull();
			Predicate nationality = root.get("nationality").isNotNull();
			query.where(builder.and(date, citizenship, nationality));
			count =  session.createQuery(query).getResultList().size();
		}
		return count;
		} catch (Exception e) {
			return count;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public ManpowerIntegrationVO fetchManpowerLogsByParams(ManpowerIntegrationVO vo, List<String> interfaceTypeCodes, List<String> manpowerLogMessageType, List<String> interfaceStatusCodes) {
		Timestamp endDate = null ;
		if (vo.getInterfaceEndDate() != null) {
			endDate = Timestamp.valueOf((vo.getInterfaceEndDate().toString().substring(0, 11)).concat(Constants.END_TIME));
		}
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select NEW com.polus.fibicomp.manpowerintegration.dto.WorkdayInterfaceLogDto(T1.awardId, T1.workdayManpowerInterfaceId, T3.title, T2.message, T2.messageType, T1.interfaceTypeCode, T4.description, T1.comments, T1.manpowerUserActionCode, T1.awardNumber, T2.manpowerLogId, T5.manpowerUserAction) from WorkdayManpowerInterface T1 ");
		hqlQuery.append(" inner join ManpowerLog T2 on T2.workdayManpowerIntefaceId = T1.workdayManpowerInterfaceId");
		hqlQuery.append(" inner join Award T3 on T3.awardId = T1.awardId");
		hqlQuery.append(" inner join ManpowerInterfaceType T4 on T4.interfaceTypeCode = T1.interfaceTypeCode");
		hqlQuery.append(" left join ManpowerUserAction T5 on T5.manpowerUserActionCode = T1.manpowerUserActionCode");
		hqlQuery.append(" where (T1.interfaceStatusCode IN (:interfaceStatusCodes) OR T1.manpowerUserActionCode is not null) and T1.interfaceTypeCode =: interfaceTypeCode");
		if (vo.getAwardNumber() != null && vo.getInterfaceStartDate() != null && endDate != null) {
			hqlQuery.append(" and T1.awardNumber =: awardNumber and T1.interfaceTimestamp >= : startDate and T1.interfaceTimestamp <= : endDate");
		} else if (vo.getInterfaceStartDate() != null && endDate != null && vo.getAwardNumber() == null) {
			hqlQuery.append(" and T1.interfaceTimestamp >= : startDate and T1.interfaceTimestamp <= : endDate");
		} else if (vo.getInterfaceStartDate() == null && endDate == null && vo.getAwardNumber() != null) {
			hqlQuery.append(" and T1.awardNumber =: awardNumber");
		}
		hqlQuery.append(" order by T1.interfaceTimestamp desc");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("interfaceStatusCodes", interfaceStatusCodes);
		query.setParameter("interfaceTypeCode", Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG);
		if (vo.getAwardNumber() != null && vo.getInterfaceStartDate() != null && endDate != null) {
			query.setParameter("awardNumber", vo.getAwardNumber());
			query.setParameter("startDate", vo.getInterfaceStartDate());
			query.setParameter("endDate", endDate);
		} else if (vo.getInterfaceStartDate() != null && endDate != null && vo.getAwardNumber() == null) {
			query.setParameter("startDate", vo.getInterfaceStartDate());
			query.setParameter("endDate", endDate);
		} else if (vo.getInterfaceStartDate() == null && endDate == null && vo.getAwardNumber() != null) {
			query.setParameter("awardNumber", vo.getAwardNumber());
		}
		
		List<WorkdayInterfaceLogDto> workdayInterfaceLogs = new ArrayList<>();
		Integer count = (query.getResultList()).size();
		vo.setPageCount(count);
		if (Boolean.TRUE.equals(vo.getIsDownload())) {
			workdayInterfaceLogs = query.getResultList();
		} else {
			workdayInterfaceLogs = query.setFirstResult((vo.getCurrentPage() - 1) * vo.getItemsPerPage()).setMaxResults(vo.getItemsPerPage()).getResultList();
		}
		vo.setWorkdayInterfaceLogDtos(workdayInterfaceLogs);
		return vo;
	}

	@Override
	public ManpowerIntegrationVO fetchManpowerTriggerDetails(ManpowerIntegrationVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<WorkdayInterfaceLogDto> interfaceLogDtos = new ArrayList<>();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_WORKDAY_MANPOWER_RETRIGGER_DETAILS(?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, vo.getAwardNumber());
				statement.setString(2, vo.getPositionId());
				statement.setString(3, vo.getPersonId());
				statement.setString(4, vo.getBudgetReferenceNumber());
				statement.setString(5, vo.getStartDate());
				statement.setString(6, vo.getEndDate());
				statement.setString(7, setManpowerFeedSortOrder(vo.getReverse(), vo.getSortBy()));
				statement.setInt(8, (vo.getCurrentPage() == null ? 0 : vo.getCurrentPage() - 1));
				statement.setInt(9, (vo.getItemsPerPage() == null ? 0 : vo.getItemsPerPage()));
				statement.setString(10, vo.getAdvancedSearch());
				statement.setString(11, vo.getTabName());
				statement.setBoolean(12, vo.getIsDownload());
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call GET_WORKDAY_MANPOWER_RETRIGGER_DETAILS (?,?,?,?,?,?,?,?,?,?,?,?,?}");
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, vo.getAwardNumber());
				statement.setString(3, vo.getPositionId());
				statement.setString(4, vo.getPersonId());
				statement.setString(5, vo.getBudgetReferenceNumber());
				statement.setString(6, vo.getStartDate());
				statement.setString(7, vo.getEndDate());
				statement.setString(8, setManpowerFeedSortOrder(vo.getReverse(), vo.getSortBy()));
				statement.setInt(9, (vo.getCurrentPage() == null ? 0 : vo.getCurrentPage() - 1));
				statement.setInt(10, (vo.getItemsPerPage() == null ? 0 : vo.getItemsPerPage()));
				statement.setString(11, vo.getAdvancedSearch());
				statement.setString(12, vo.getTabName());
				statement.setBoolean(13, vo.getIsDownload());
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				WorkdayInterfaceLogDto workdayInterfaceLogDto = new WorkdayInterfaceLogDto();
				if (vo.getTabName().equals("AWARD_MANPOWER_RETRIGGER")) {
					workdayInterfaceLogDto.setWorkdayManpowerInterfaceId(resultSet.getInt("WORKDAY_MANPOWER_INTERFACE_ID"));
					workdayInterfaceLogDto.setAwardId(resultSet.getInt("AWARD_ID"));
					workdayInterfaceLogDto.setAwardNumber(resultSet.getString("AWARD_NUMBER"));
					workdayInterfaceLogDto.setAwardTitle(resultSet.getString("TITLE"));
					workdayInterfaceLogDto.setErrorMessage(resultSet.getString("MESSAGE"));
					workdayInterfaceLogDto.setMessageType(resultSet.getString("MESSAGE_TYPE"));
					workdayInterfaceLogDto.setInterfaceTypeCode(resultSet.getString("INTERFACE_TYPE_CODE"));
					workdayInterfaceLogDto.setUserActionName(resultSet.getString("MANPOWER_USER_ACTION"));
					interfaceLogDtos.add(workdayInterfaceLogDto);
				} else {
					workdayInterfaceLogDto.setAwardNumber(resultSet.getString("AWARD_NUMBER"));
					workdayInterfaceLogDto.setPositionId(resultSet.getString("POSITION_ID"));
					workdayInterfaceLogDto.setWorkdayManpowerInterfaceId(resultSet.getInt("WORKDAY_MANPOWER_INTERFACE_ID"));
					workdayInterfaceLogDto.setBudgetReferenceNumber(resultSet.getString("BUDGET_REFERENCE_NUMBER"));
					workdayInterfaceLogDto.setInterfaceTypeCode(resultSet.getString("INTERFACE_TYPE_CODE"));
					workdayInterfaceLogDto.setInterfaceTypeName(resultSet.getString("INTERFACE_TYPE"));
					workdayInterfaceLogDto.setInterfaceStatusCode(resultSet.getString("INTERFACE_STATUS_CODE"));
					workdayInterfaceLogDto.setInterfaceStatusName(resultSet.getString("INTERFACE_STATUS"));
					workdayInterfaceLogDto.setResourceUniqueId(resultSet.getString("RESOURCE_UNIQUE_ID"));
					workdayInterfaceLogDto.setResourceName(resultSet.getString("FULL_NAME"));
					workdayInterfaceLogDto.setChargeStartDate(resultSet.getTimestamp("START_DATE"));
					workdayInterfaceLogDto.setChargeEndDate(resultSet.getTimestamp("END_DATE"));
					workdayInterfaceLogDto.setJobProfileType(resultSet.getString("JOB_PROFILE_TYPE"));
					workdayInterfaceLogDto.setUserActionName(resultSet.getString("MANPOWER_USER_ACTION"));
					workdayInterfaceLogDto.setPersonId(resultSet.getString("PERSON_ID"));
					workdayInterfaceLogDto.setCostAllocation(resultSet.getBigDecimal("COST_ALLOCATION"));
					workdayInterfaceLogDto.setPositionStatus(resultSet.getString("POSITION_STATUS"));
					workdayInterfaceLogDto.setComments(resultSet.getString("COMMENTS"));
					workdayInterfaceLogDto.setManpowerLogId(resultSet.getInt("MANPOWER_LOG_ID"));
					interfaceLogDtos.add(workdayInterfaceLogDto);
				}
			}
			vo.setWorkdayInterfaceLogDtos(interfaceLogDtos);
			vo.setPageCount(getManpowerInterfaceCount(vo));
		} catch (SQLException e) {
			logger.info("Exception in fetchManpowerTriggerDetails : ", e);
			e.printStackTrace();
		}
		return vo;
	}

	private String setManpowerFeedSortOrder(String order, String sortBy) {
		String sortOrder = null;
		if (sortBy != null && !sortBy.isEmpty()) {	
			switch (sortBy) {
			case "positionId":
				sortOrder = " T.POSITION_ID "+ order;
				break;
			case "awardNumber":
				sortOrder = " T.AWARD_NUMBER "+ order;
				break;
			case "resourceName":
				sortOrder = " T.FULL_NAME "+ order;
				break;
			case "chargeStartDate":
				sortOrder = " START_DATE "+ order;
				break;
			case "chargeEndDate":
				sortOrder = " END_DATE "+ order;
				break;
			case "jobProfile":
				sortOrder = " T.JOB_PROFILE_TYPE "+ order;
				break;
			case "positionStatus":
				sortOrder = " T.POSITION_STATUS "+ order;
				break;
			case "interfaceStatus":
				sortOrder = " T.INTERFACE_STATUS "+ order;
				break;
			case "personId":
				sortOrder = " T.PERSON_ID "+ order;
				break;
			case "budgetReferenceNumber":
				sortOrder = " T.BUDGET_REFERENCE_NUMBER "+ order;
				break;
			case "costAllocation":
				sortOrder = " T.COST_ALLOCATION "+ order;
				break;
			case "jobProfileType":
				sortOrder = " T.JOB_PROFILE_TYPE "+ order;
				break;
			case "title":
				sortOrder = " T.TITLE "+ order;
				break;
			case "error":
				sortOrder = " T.MESSAGE "+ order;
				break;
			case "errorType":
				sortOrder = " T.MESSAGE_TYPE "+ order;
				break;
			default:
				break;
			}
		}
		return sortOrder;
				
	}

	private Integer getManpowerInterfaceCount(ManpowerIntegrationVO vo) {
		Integer count = 0;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_WORKDAY_MANPOWER_RETRIGGER_COUNT(?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, vo.getAwardNumber());
				statement.setString(2, vo.getPositionId());
				statement.setString(3, vo.getPersonId());
				statement.setString(4, vo.getBudgetReferenceNumber());
				statement.setString(5, vo.getStartDate());
				statement.setString(6, vo.getEndDate());
				statement.setString(7, setManpowerFeedSortOrder(vo.getReverse(), vo.getSortBy()));
				statement.setInt(8, (vo.getCurrentPage() == null ? 0 : vo.getCurrentPage() - 1));
				statement.setInt(9, (vo.getItemsPerPage() == null ? 0 : vo.getItemsPerPage()));
				statement.setString(10, vo.getAdvancedSearch());
				statement.setString(11, vo.getTabName());
				statement.setBoolean(12, true);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureNameForCount = "GET_WORKDAY_MANPOWER_RETRIGGER_COUNT";
				String functionCall = "{call " + procedureNameForCount + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, vo.getAwardNumber());
				statement.setString(3, vo.getPositionId());
				statement.setString(4, vo.getPersonId());
				statement.setString(5, vo.getBudgetReferenceNumber());
				statement.setString(6, vo.getStartDate());
				statement.setString(7, vo.getEndDate());
				statement.setString(8, setManpowerFeedSortOrder(vo.getReverse(), vo.getSortBy()));
				statement.setInt(9, (vo.getCurrentPage() == null ? 0 : vo.getCurrentPage() - 1));
				statement.setInt(10, (vo.getItemsPerPage() == null ? 0 : vo.getItemsPerPage()));
				statement.setString(11, vo.getAdvancedSearch());
				statement.setString(12, vo.getTabName());
				statement.setBoolean(13, true);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.info("exception in getManpowerInterfaceCount : " + e);
			e.printStackTrace();
		}
		return count;
	}

	private String setSortOrder(Map<String, String> sort) {
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<WorkdayInterfaceLogDto> fetchPositionErrorDetailsByParams(String resourceUniqueId, List<String> interfaceTypeCodes, List<String> manpowerInterfaceStatuses) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select new com.polus.fibicomp.manpowerintegration.dto.WorkdayInterfaceLogDto(T1.awardId,T1.workdayManpowerInterfaceId, T3.title as awardTitle, T2.message as errorMessage,  T2.messageType,  T1.interfaceTypeCode,  T4.description as interfaceTypeName, T1.comments,  T1.manpowerUserActionCode as userActionCode,  T1.awardNumber, T2.manpowerLogId, T5.manpowerUserAction) from WorkdayManpowerInterface T1 ");
		hqlQuery.append(" inner join ManpowerLog T2 on T2.workdayManpowerIntefaceId = T1.workdayManpowerInterfaceId ");
		hqlQuery.append(" inner join Award T3 on T3.awardId = T1.awardId ");
		hqlQuery.append(" inner join ManpowerInterfaceType T4 on T4.interfaceTypeCode = T1.interfaceTypeCode ");
		hqlQuery.append(" left outer join ManpowerUserAction T5 on T5.manpowerUserActionCode = T1.manpowerUserActionCode ");
		hqlQuery.append("  where (T1.interfaceStatusCode IN (:manpowerInterfaceStatuses) OR T1.manpowerUserActionCode is not null) and (T1.interfaceTypeCode in (:interfaceTypeCode) or (T1.interfaceTypeCode = '3' and T2.messageType = 'TECHNICAL_ERROR')) and T1.resourceUniqueId =: resourceUniqueId");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("manpowerInterfaceStatuses", manpowerInterfaceStatuses);
		query.setParameter("interfaceTypeCode", interfaceTypeCodes);
		query.setParameter("resourceUniqueId", resourceUniqueId);
		return query.getResultList();
	}

	@Override
	public List<AwardManpowerResource> getLessCostAllocationResources() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<AwardManpowerResource> awardManpowerResources = new ArrayList<>();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call WORKDAY_PARTIAL_ALLOC_REPORT()}");
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "WORKDAY_PARTIAL_ALLOC_REPORT";
				String functionCall = "{call " + procedureName + "(?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet != null && resultSet.next()) {
				AwardManpowerResource resource = new AwardManpowerResource();
				resource.setAwardNumber(resultSet.getString("AWARD_NUMBER"));
				resource.setBudgetReferenceNumber(resultSet.getString("BUDGET_REFERENCE_NUMBER"));
				resource.setPlanStartDate(resultSet.getTimestamp("PLAN_START_DATE"));
				resource.setPlanEndDate(resultSet.getTimestamp("PLAN_END_DATE"));
				resource.setChargeStartDate(resultSet.getTimestamp("CHARGE_START_DATE"));
				resource.setChargeEndDate(resultSet.getTimestamp("CHARGE_END_DATE"));
				resource.setCostAllocation(resultSet.getBigDecimal("COST_ALLOCATION"));
				resource.setPersonId(resultSet.getString("PERSON_ID"));
				resource.setPositionId(resultSet.getString("POSITION_ID"));
				resource.setFullName(resultSet.getString("FULL_NAME"));
				resource.setDepartment(resultSet.getString("CAMPUS"));
				resource.setPositionStatus(resultSet.getString("POSITION_STATUS"));
				resource.setDescription(resultSet.getString("DESCRIPTION"));
				resource.setIsRemainingCAFromWBS(resultSet.getBoolean("IS_REMAINING_CA_FROM_WBS"));
				resource.setUpdateTimestamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				resource.setSubmitUser(resultSet.getString("SUBMIT_USER"));
				resource.setAwardId(resultSet.getInt("AWARD_ID"));
				resource.setWorkdayManpowerInterfaceId(resultSet.getInt("WORKDAY_MANPOWER_INTERFACE_ID"));
				resource.setPiName(resultSet.getString("PI_NAME"));
				resource.setLeadUnit(resultSet.getString("UNIT_NAME"));
				awardManpowerResources.add(resource);
			}
		} catch (SQLException e) {
			logger.error("Error in getLessCostAllocationResources {}", e.getMessage());
		}
		return awardManpowerResources;
	}

	@Override
	public List<ManpowerUserAction> fetchManpowerUserActions() {
		return hibernateTemplate.loadAll(ManpowerUserAction.class);
	}

	@Override
	public Integer getPositionCountWithCreateUser(String positionId) {
		try {
			Integer count = 0;
			Query query = entityManager.createNativeQuery("SELECT COUNT(distinct POSITION_ID) FROM AWARD_MANPOWER_RESOURCE WHERE POSITION_ID = :positionId and CREATE_USER ='quickstart'");
			query.setParameter("positionId", positionId);
			count = ((Number) query.getSingleResult()).intValue();
			return count;
		} catch (Exception e) {
			logger.error("error in getPositionCountWithCreateUser {}", e.getMessage());
			return null;
		}
	}

	@Override
	public List<AwardManpowerResource> getOwnedMigratedAwardManpowerResourcesByAwardNumber(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardManpowerResource> query = builder.createQuery(AwardManpowerResource.class);
		Root<AwardManpowerResource> rootAwardManpowerResource = query.from(AwardManpowerResource.class);
		Predicate predicateAwardNumber = builder.equal(rootAwardManpowerResource.get("awardNumber"), awardNumber);
		Predicate predicatePIPersonId = rootAwardManpowerResource.get("positionId").isNotNull();
		Predicate predicatePersonId = rootAwardManpowerResource.get("personId").isNull();
		Predicate predicateExpiredDummyPerson = builder.notEqual(rootAwardManpowerResource.get("personId"),"999999999100");
		Predicate predicatePerson = builder.or(predicatePersonId, predicateExpiredDummyPerson);
		query.where(builder.and(predicateAwardNumber, predicatePIPersonId, predicatePerson));
		query.multiselect(rootAwardManpowerResource.get("personId"), rootAwardManpowerResource.get("positionId"),
				rootAwardManpowerResource.get("awardNumber")).distinct(true);
		return session.createQuery(query).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<WorkdayInterfaceLogDto> getOtherCurrentCostAllocationDetails(String personId, String resourceUniqueId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select new com.polus.fibicomp.manpowerintegration.dto.WorkdayInterfaceLogDto(T1.awardNumber,  T1.planStartDate,  T1.planEndDate, T1.chargeStartDate, T1.chargeEndDate, T1.positionOwnedByAward, T1.costAllocation, T4.description as positionStatus, T1.fullName as personFullName, T1.positionId, T1.personId, T5.description as jobProfileType, T2.budgetReferenceNumber) from AwardManpowerResource T1 ");
		hqlQuery.append(" inner join AwardManpower T2 on T2.awardManpowerId = T1.awardManpowerId ");
		hqlQuery.append(" inner join Award T3 on T3.awardId = T2.awardId ");
		hqlQuery.append(" inner join ManpowerPositionStatus T4 on T4.positionStatusCode = T1.positionStatusCode ");
		hqlQuery.append(" left outer join ManpowerJobProfileType T5 on T5.jobProfileTypeCode = T1.jobProfileTypeCode ");
		hqlQuery.append("  where T3.awardSequenceStatus = : awardSequenceStatus and T1.personId =: personId and T1.resourceUniqueId <> : resourceUniqueId");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("awardSequenceStatus", Constants.AWARD_FINAL_STATUS_ACTIVE);
		query.setParameter("personId", personId);
		query.setParameter("resourceUniqueId", resourceUniqueId);
		return query.getResultList();
	}

	@Override
	public String fetchManpowerUserActionNameById(String manpowerUserActionCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "select manpowerUserAction from ManpowerUserAction WHERE manpowerUserActionCode = :manpowerUserActionCode";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("manpowerUserActionCode", manpowerUserActionCode);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			return "";
		}
	}

	@Override
	public Integer getInterfaceErrorCountByResourceUniqueId(String resourceUniqueId) {
		Integer count = 0;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String maxHQLQuery = "SELECT count(*) FROM WorkdayManpowerInterface T1 inner join ManpowerLog T2 on T2.workdayManpowerIntefaceId = T1.workdayManpowerInterfaceId  WHERE T1.resourceUniqueId=:resourceUniqueId and T1.interfaceStatusCode = 2";
		Query countQuery = session.createQuery(maxHQLQuery);
		countQuery.setParameter("resourceUniqueId", resourceUniqueId);
		count = Integer.parseInt(countQuery.getSingleResult().toString());
		return count;
	}

	@Override
	public ManpowerInterfaceStatus fetchManpowerInterfaceStatusById(String manpowerInterfaceStatusCode) {
		return hibernateTemplate.get(ManpowerInterfaceStatus.class, manpowerInterfaceStatusCode);
	}

	@Override
	public List<WorkdayInterfaceLogDto> getWorkdayInterfaceLogDtosByWorkdayManpowerInterfaceId(Integer workdayManpowerInterfaceId) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public WorkdayManpowerInterface fetchManpowerInterfaceByAwardNumberAndInterfaceTypeAndInterfaceStatus(String awardNumber, String manpowerInterfaceType, String manpowerInterfaceStatus) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<WorkdayManpowerInterface> query = builder.createQuery(WorkdayManpowerInterface.class);
			Root<WorkdayManpowerInterface> rootManpowerInterface= query.from(WorkdayManpowerInterface.class);
			Predicate predicateInterfaceStatus= builder.equal(rootManpowerInterface.get("interfaceStatusCode"), manpowerInterfaceStatus);
			Predicate predicateInterfaceType= builder.equal(rootManpowerInterface.get("interfaceTypeCode"), manpowerInterfaceType);
			Predicate predicateAwardNumber = builder.equal(rootManpowerInterface.get("awardNumber"), awardNumber);
			query.where(builder.and(predicateInterfaceStatus, predicateAwardNumber, predicateInterfaceType));
			query.orderBy(builder.asc(rootManpowerInterface.get("workdayManpowerInterfaceId")));
			return session.createQuery(query).uniqueResult();
		} catch (Exception e ) {
			logger.error("Error in fetchManpowerInterfaceByAwardNumberAndInterfaceTypeAndInterfaceStatus {}", e.getMessage());
			return null;
		}
	}

    @Override
	public WorkdayManpowerInterface getLatestResourceInterfaceByAwardNumberAndInterfaceType(String awardNumber, String interfaceType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM WorkdayManpowerInterface WHERE workdayManpowerInterfaceId = (select max(workdayManpowerInterfaceId) from WorkdayManpowerInterface where awardNumber =:awardNumber and interfaceTypeCode = :interfaceType)";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("awardNumber", awardNumber);
		query.setParameter("interfaceType", interfaceType);
		if (query.getResultList() != null && !(query.getResultList()).isEmpty()) {
			return (WorkdayManpowerInterface) query.getSingleResult();
		}
		return null;
	}

	@Override
	public RiseErrorAllocations getRiseErrorAllocationByResourceUniqueId(String resourceUniqueId) {
		return hibernateTemplate.get(RiseErrorAllocations.class, resourceUniqueId);
	}

	@Override
	public void saveRiseErrorAllocation(RiseErrorAllocations allocation) {
		try {
			hibernateTemplate.saveOrUpdate(allocation);
		} catch (Exception e) {
			logger.error("Exception in saveRiseErrorAllocation {}", e.getMessage());
		}
	}

	@Override
	public void deleteRiseErrorAllocation(RiseErrorAllocations riseErrorAllocation) {
		hibernateTemplate.delete(riseErrorAllocation);
		logger.info("Rise Error Allocation deleted");
  }
  
	public AwardPerson getAwardPIPersonByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM AwardPerson P WHERE P.awardId = : awardId and P.isPi = 'Y'";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("awardId", awardId);
		if (query.getResultList() != null && !(query.getResultList()).isEmpty()) {
			return (AwardPerson) query.getSingleResult();
		}
		return null;
	}

	@Override
	public AwardManpowerResource getClosePositionResourceByPositionId(String positionId) {		
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select T3 From Award T1 inner join AwardManpower T2 on T2.awardId = T1.awardId");
		hqlQuery.append(" inner join AwardManpowerResource T3 on T3.awardManpowerId = T2.awardManpowerId");
		hqlQuery.append("  where T1.awardSequenceStatus in ('ACTIVE') and T3.positionId = : positionId");
		Query findManpowerResources = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		findManpowerResources.setParameter("positionId", positionId);
		return (AwardManpowerResource) findManpowerResources.getResultList().get(0);
	}

	@Override
	public String getCampusByUnit(String unitNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		String result = "N";
		try {
			String functionName = "FN_CAMPUS_FOR_UNIT";
			String functionCall = "{ ? = call " + functionName + "(?) }";
			statement = connection.prepareCall(functionCall);
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setString(2, unitNumber);
			statement.execute();
			return statement.getString(1);
		} catch (SQLException e) {
			logger.error("error in getAwardManpowerCostAllocation {}", e.getMessage());
		}
		return result;
	}

}
