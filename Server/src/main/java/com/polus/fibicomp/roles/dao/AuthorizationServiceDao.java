package com.polus.fibicomp.roles.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "authorizationServiceDao")
public class AuthorizationServiceDao {

	protected static Logger logger = LogManager.getLogger(AuthorizationServiceDao.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	public ArrayList<String> allSystemLevelPermission(String personId) {
		ArrayList<String> rightList = new ArrayList<String>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_ALL_SYSTEM_RIGHTS(?)}");
				statement.setString(1, personId);
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call GET_ALL_SYSTEM_RIGHTS(?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setString(1, personId);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
			while (rset.next()) {
				rightList.add(rset.getString("RIGHT_NAME").toString());
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		if (rightList.isEmpty()) {
			rightList.add("No data is available");
		}
		return rightList;
	}

	public ArrayList<String> allDepartmentLevelPermission(Integer moduleCode, String personId, String leadUnit, Integer moduleItemKey) {
		ArrayList<String> departmentLevelrightList = new ArrayList<String>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		if (moduleItemKey == null)
			moduleItemKey = -1;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_ALL_RIGHTS_FOR_A_MODULE(?,?,?,?)}");
				statement.setInt(1, moduleCode);
				statement.setString(2, personId);
				statement.setString(3, leadUnit);
				statement.setInt(4, moduleItemKey);
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call GET_ALL_RIGHTS_FOR_A_MODULE(?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setInt(1, moduleCode);
				statement.setString(2, personId);
				statement.setString(3, leadUnit);
				statement.setInt(4, moduleItemKey);
				statement.registerOutParameter(5, OracleTypes.CURSOR);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
			while (rset != null && rset.next()) {
				departmentLevelrightList.add(rset.getString("RIGHT_NAME").toString());
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		if (departmentLevelrightList.isEmpty()) {
			departmentLevelrightList.add("No data is available");
		}
		return departmentLevelrightList;
	}

	public boolean isPersonHasRightInDepartment(String rightName, String personId, String unitNumber) {
		return isPersonHasRight(rightName, personId, unitNumber, null, null, "RIGHT_IN_DEPT");
	}

	public boolean isPersonHasRightInaDocument(String rightName, String personId, Integer moduleCode,
			String moduleItemKey) {
		return isPersonHasRight(rightName, personId, null, moduleCode, moduleItemKey, "RIGHT_IN_A_DOCUMENT");
	}

	public boolean isPersonHasRightInAnyDepartment(String rightName, String personId) {
		return isPersonHasRight(rightName, personId, null, null, null, "RIGHT_IN_ANY_DEPT");
	}

	private boolean isPersonHasRight(String rightName, String personId, String unitNumber, Integer moduleCode, String moduleItemKey, String type) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			String functionName = "FN_PERSON_HAS_RIGHTS";
			String functionCall = "{ ? = call " + functionName + "(?,?,?,?,?,?) }";
			statement = connection.prepareCall(functionCall);
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setString(2, rightName);
			statement.setString(3, personId);
			statement.setString(4, unitNumber);
			statement.setInt(5, moduleCode);
			statement.setString(6, moduleItemKey);
			statement.setString(7, type);
			statement.execute();
			int result = statement.getInt(1);
			if (result == 1) {
				return true;
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return false;
	}

}
