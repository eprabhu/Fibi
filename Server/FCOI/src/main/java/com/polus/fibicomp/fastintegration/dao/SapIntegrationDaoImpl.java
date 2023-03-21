package com.polus.fibicomp.fastintegration.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.List;

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

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fastintegration.pojo.SapCostCenter;
import com.polus.fibicomp.fastintegration.pojo.SapFundCenter;
import com.polus.fibicomp.fastintegration.pojo.SapGrantCode;
import com.polus.fibicomp.fastintegration.pojo.SapIntegrationLog;
import com.polus.fibicomp.fastintegration.pojo.SapProfitCenter;
import com.polus.fibicomp.manpower.pojo.ManpowerJobProfileType;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "sapIntegrationDao")
public class SapIntegrationDaoImpl implements SapIntegrationDao {

	protected static Logger logger = LogManager.getLogger(SapIntegrationDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;
	
	@Value("${oracledb}")
	private String oracledb;

	private static final String DESCRIPTION = "description";
	private static final String ACTIVE = "isActive";

	@Override
	public List<SapProfitCenter> findProfitCenter(String searchString) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapProfitCenter> query = builder.createQuery(SapProfitCenter.class);
			Root<SapProfitCenter> rootProfitCenter = query.from(SapProfitCenter.class);			
			Predicate predicateProfitCenterCode = builder.like(builder.lower(rootProfitCenter.get("profitCenterCode")), "%" + searchString.toLowerCase() + "%");
			Predicate predicateProfitCenterName = builder.like(builder.lower(rootProfitCenter.get("profitCenterName")), "%" + searchString.toLowerCase() + "%");
			Predicate predicateDescription = builder.like(builder.lower(rootProfitCenter.get(DESCRIPTION)), "%" + searchString.toLowerCase() + "%");
			Predicate predicateActive = builder.equal(rootProfitCenter.get(ACTIVE), true);
			query.where(builder.and(builder.or(predicateProfitCenterCode, predicateProfitCenterName, predicateDescription), predicateActive));
			query.orderBy(builder.asc(rootProfitCenter.get("profitCenterName")));
			return session.createQuery(query).setMaxResults(25).getResultList();
		});
	}

	@Override
	public List<SapFundCenter> findFundCenter(String searchString) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapFundCenter> query = builder.createQuery(SapFundCenter.class);
			Root<SapFundCenter> rootFundCenter = query.from(SapFundCenter.class);			
			Predicate predicateFundCenterCode = builder.like(builder.lower(rootFundCenter.get("fundCenterCode")), "%" + searchString.toLowerCase() + "%");
			Predicate predicateFundCenterName = builder.like(builder.lower(rootFundCenter.get("fundCenterName")), "%" + searchString.toLowerCase() + "%");
			Predicate predicateDescription = builder.like(builder.lower(rootFundCenter.get(DESCRIPTION)), "%" + searchString.toLowerCase() + "%");
			Predicate predicateActive = builder.equal(rootFundCenter.get(ACTIVE), true);
			query.where(builder.and(builder.or(predicateFundCenterCode, predicateFundCenterName, predicateDescription), predicateActive));
			query.orderBy(builder.asc(rootFundCenter.get("fundCenterName")));
			return session.createQuery(query).setMaxResults(25).getResultList();
		});
	}

	@Override
	public List<SapCostCenter> findCostCenter(String searchString) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapCostCenter> query = builder.createQuery(SapCostCenter.class);
			Root<SapCostCenter> rootCostCenter = query.from(SapCostCenter.class);				
			Predicate predicateCostCenterCode = builder.like(builder.lower(rootCostCenter.get("costCenterCode")), "%" + searchString.toLowerCase() + "%");
			Predicate predicateCostCenterName = builder.like(builder.lower(rootCostCenter.get("costCenterName")), "%" + searchString.toLowerCase() + "%");
			Predicate predicateDescription = builder.like(builder.lower(rootCostCenter.get(DESCRIPTION)), "%" + searchString.toLowerCase() + "%");
			Predicate predicateActive = builder.equal(rootCostCenter.get(ACTIVE), true);
			query.where(builder.and(builder.or(predicateCostCenterCode, predicateCostCenterName, predicateDescription), predicateActive));
			query.orderBy(builder.asc(rootCostCenter.get("costCenterName")));
			return session.createQuery(query).setMaxResults(25).getResultList();
		});
	}

	@Override
	public List<SapGrantCode> findGrantCode(String searchString) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapGrantCode> query = builder.createQuery(SapGrantCode.class);
			Root<SapGrantCode> rootGrantCode = query.from(SapGrantCode.class);				
			Predicate predicateGrantCode = builder.like(builder.lower(rootGrantCode.get("grantCode")), "%" + searchString.toLowerCase() + "%");
			Predicate predicateGrantName = builder.like(builder.lower(rootGrantCode.get("grantCodeName")), "%" + searchString.toLowerCase() + "%");
			Predicate predicateActive = builder.equal(rootGrantCode.get(ACTIVE), true);
			query.where(builder.and(builder.or(predicateGrantCode, predicateGrantName), predicateActive));
			query.orderBy(builder.asc(rootGrantCode.get("grantCodeName")));
			return session.createQuery(query).setMaxResults(25).getResultList();
		});
	}

	@Override
	public String getWorkdayConfigurationData(String oauthApi) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT W.configurationValue FROM WorkdayConfigurationData W where W.configurationKey =:oauthApi";		
		Query query = session.createQuery(hqlQuery);
		query.setParameter("oauthApi", oauthApi);
		return (String) query.getSingleResult();
	}

	@Override
	public SapProfitCenter getSapProfitCenter(String profitCenterCode) {
		return hibernateTemplate.get(SapProfitCenter.class, profitCenterCode);
	}

	@Override
	public void saveOrUpdateSapProfitCenter(SapProfitCenter sapProfitCenter) {
		hibernateTemplate.saveOrUpdate(sapProfitCenter);
	}

	@Override
	public void saveSapIntergrationLog(SapIntegrationLog integrationLog) {
		hibernateTemplate.saveOrUpdate(integrationLog);
	}

	@Override
	public SapFundCenter getSapFundCenter(String sapFundCenterCode) {
		return hibernateTemplate.get(SapFundCenter.class, sapFundCenterCode);
	}

	@Override
	public void saveOrUpdateSapFundCenter(SapFundCenter sapFundCenter) {
		hibernateTemplate.saveOrUpdate(sapFundCenter);
	}

	@Override
	public SapCostCenter getSapCostCenter(String costCenterCode) {
		return hibernateTemplate.get(SapCostCenter.class, costCenterCode);
	}

	@Override
	public void saveOrUpdateSapCostCenter(SapCostCenter sapCostCenter) {
		hibernateTemplate.saveOrUpdate(sapCostCenter);
	}

	@Override
	public Integer callToInactiveFeed(Timestamp currentTime, String sapIntegration) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
				statement = connection.prepareCall("{call DEACTIVATE_INACTIVE_FEEDS(?,?)}");
				statement.setString(1, sapIntegration);
				statement.setTimestamp(2, currentTime);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
				String procedureName = "DEACTIVATE_INACTIVE_FEEDS";
				String functionCall = "{call " + procedureName + "(?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, sapIntegration);
				statement.setTimestamp(3, currentTime);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			if (resultSet != null) {
				while (resultSet.next()) {
					return resultSet.getInt("DELETED_FEED_COUNT");
				}
			}
		} catch (Exception e) {
			logger.error("Exception in callToInactiveFeed : {}", e.getMessage());
		}
		return null;
	}

	@Override
	public SapGrantCode getGrantCode(String grantNumber) {
		return hibernateTemplate.get(SapGrantCode.class, grantNumber);
	}

	@Override
	public void saveOrUpdateGrantCode(SapGrantCode sapGrantCode) {
		hibernateTemplate.saveOrUpdate(sapGrantCode);
		
	}

}
