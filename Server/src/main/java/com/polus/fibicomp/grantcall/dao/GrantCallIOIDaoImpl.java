package com.polus.fibicomp.grantcall.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Predicate;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.grantcall.pojo.GrantCallIOIHeader;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIMembers;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "grantCallIOIDao")
public class GrantCallIOIDaoImpl implements GrantCallIOIDao {

	protected static Logger logger = LogManager.getLogger(GrantCallIOIDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Override
	public GrantCallIOIHeader saveOrUpdateGrantCallIOI(GrantCallIOIHeader grantCallIOI) {
		hibernateTemplate.saveOrUpdate(grantCallIOI);
		return grantCallIOI;
	}

	@Override
	public void deleteIOIMember(GrantCallIOIMembers grantCallIOIMember) {
		try {
			hibernateTemplate.delete(grantCallIOIMember);
		} catch (Exception e) {
			logger.error("Exception while deleting IOIMember {}" , e.getMessage());
		}
	}

	@Override
	public GrantCallIOIMembers fetchIOIMember(Integer grantIOIMemberId) {
		return hibernateTemplate.get(GrantCallIOIMembers.class, grantIOIMemberId);
	}

	@Override
	public List<GrantCallIOIMembers> fetchIOIMembersBasedOnIOIId(Integer grantCallIOIId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallIOIMembers> query = builder.createQuery(GrantCallIOIMembers.class);
		Root<GrantCallIOIMembers> rootIOIMember = query.from(GrantCallIOIMembers.class);
		query.where(builder.equal(rootIOIMember.get("grantCallIOIId"), grantCallIOIId));
		return session.createQuery(query).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public GrantCallIOIHeader fetchGrantIOIByIOIId(Integer grantIOIId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM GrantCallIOIHeader WHERE grantCallIOIId=:grantIOIId";
		Query<GrantCallIOIHeader> query = session.createQuery(hqlQuery);
		query.setParameter("grantIOIId", grantIOIId);
		if (query.getResultList() != null && !(query.getResultList()).isEmpty()) {
			return query.getSingleResult();
		}
		return null;
	}

	@Override
	public void deleteGrantCallIOI(GrantCallIOIHeader grantIOIHeader) {
		try {
			hibernateTemplate.delete(grantIOIHeader);
		} catch (Exception e) {
			logger.error("Exception while deleting GrantCallIOI {}" , e.getMessage());
		}
	}

	@Override
	public GrantCallIOIMembers saveOrUpdateIOIMembers(GrantCallIOIMembers grantCallIOIMember) {
		hibernateTemplate.saveOrUpdate(grantCallIOIMember);
		return grantCallIOIMember;
	}

//	@SuppressWarnings({ "unchecked", "rawtypes" })
//	@Override
//	public Set<String> fetchPersonIdByUnitIdAndRoleId(String unitNumber, Integer roleId) {
//		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
//		Set<String> recipientSet = new HashSet();
// 		String hqlQuery = "SELECT T.personId FROM PersonRoles T WHERE T.unitNumber = :unitNumber and T.roleId = :roleId";
//		Query query = session.createQuery(hqlQuery);
//		query.setParameter("unitNumber", unitNumber);
//		query.setParameter("roleId",roleId);
//		if (query.getResultList() != null && !query.getResultList().isEmpty()) {
//			recipientSet.addAll(query.getResultList());
//		}	
//        return recipientSet;
//	}

	@Override
	public List<GrantCallIOIHeader> getDashboardDataForIOI(Integer grantCallId, String personId, String tabName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<GrantCallIOIHeader> grantCallIOIs = new ArrayList<GrantCallIOIHeader>();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_IOI_DASHBOARD(?,?,?)}");
				statement.setInt(1, grantCallId);
				statement.setString(2, personId);
				statement.setString(3, tabName);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_IOI_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, grantCallId);
				statement.setString(3, personId);
				statement.setString(4, tabName);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				GrantCallIOIHeader grantCallIOI = new GrantCallIOIHeader();
				grantCallIOI.setSubmittingUnitName(resultSet.getString("SUBMITING_UNIT_NAME"));
				grantCallIOI.setPiFullName(resultSet.getString("PI_FULL_NAME"));
				grantCallIOI.setProjectTitle(resultSet.getString("PROJECT_TITLE"));
				grantCallIOI.setRequestedDirectCost(resultSet.getBigDecimal("REQUESTED_DIRECT_COST"));
				grantCallIOI.setUpdateUserFullName(resultSet.getString("UPDATE_USER"));
				grantCallIOI.setMemberCount(resultSet.getInt("PERSON_COUNT"));
				grantCallIOI.setGrantCallIOIId(resultSet.getInt("IOI_ID"));
				grantCallIOI.setGrantIOIStatusCode(resultSet.getInt("STATUS_CODE"));
				grantCallIOI.setCreateUser(resultSet.getString("CREATE_USER"));
				grantCallIOI.setUnitNumber(resultSet.getString("UNIT_NUMBER"));
				grantCallIOIs.add(grantCallIOI);
			}
		} catch (SQLException e) {
			logger.error("exception in dashboardIOIs : {}", e.getMessage());
		}
		return grantCallIOIs;
	}

	@Override
	public List<GrantCallIOIHeader> fetchSubmittedGrantCallIOIByGrantCallId(Integer grantCallId, Integer grantIOIStatusCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallIOIHeader> query = builder.createQuery(GrantCallIOIHeader.class);
		Root<GrantCallIOIHeader> rootGrantCallIOIHeader = query.from(GrantCallIOIHeader.class);
		Predicate predicateOne = builder.equal(rootGrantCallIOIHeader.get("grantCallId"), grantCallId);
		Predicate predicateTwo = builder.equal(rootGrantCallIOIHeader.get("grantIOIStatusCode"), grantIOIStatusCode);
		query.where(builder.and(predicateOne, predicateTwo));
		return session.createQuery(query).getResultList();
	}

}
