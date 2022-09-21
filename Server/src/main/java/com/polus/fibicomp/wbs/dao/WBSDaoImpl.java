package com.polus.fibicomp.wbs.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;

import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.constants.Constants;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "wbsDao")
public class WBSDaoImpl implements WBSDao{

	protected static Logger logger = LogManager.getLogger(WBSDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Override
	public String generateWBSNumber(Integer awardId, String isGenerateAccountNumber, Integer budgetDetailId,
			String budgetCategoryCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GENERATE_AWARD_WBS_NUMBER(?,?,?,?)}");
				statement.setInt(1, awardId);
				statement.setInt(2, budgetDetailId == null ? 0 : budgetDetailId);
				statement.setString(3, budgetCategoryCode);
				statement.setString(4, isGenerateAccountNumber);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GENERATE_AWARD_WBS_NUMBER";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setInt(1, awardId);
				statement.setInt(2, budgetDetailId == null ? 0 : budgetDetailId);
				statement.setString(3, budgetCategoryCode);
				statement.setString(4, isGenerateAccountNumber);
				statement.registerOutParameter(5, OracleTypes.CURSOR);
				statement.executeUpdate();
				resultSet = (ResultSet) statement.getObject(6);
			}
			while (resultSet.next()) {
				return resultSet.getString(1);
			}
		} catch (SQLException e) {
			logger.info("Error ocuured in generateWBSNumber : {}", e.getMessage());
			throw new ApplicationException("Error occurred in generateWBSNumber", e, Constants.DB_PROC_ERROR);
		}
		return null;
	}

	@Override
	public String generateIOCode(Integer awardId,  Integer budgetHeaderId,String flag,String costElement,Integer budgetPeriodId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GENERATE_IO_CODE(?,?,?,?,?)}");
				statement.setInt(1, awardId);
				statement.setInt(2, budgetHeaderId == null ? 0 : budgetHeaderId);
				statement.setString(3, flag);
				statement.setString(4, costElement);
				statement.setInt(5, budgetPeriodId == null ? 0 : budgetPeriodId);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GENERATE_IO_CODE";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setInt(1, awardId);
				statement.setInt(2, budgetHeaderId);
				statement.setString(3, flag);
				statement.setString(4, costElement);
				statement.setInt(5, budgetPeriodId == null ? 0 : budgetPeriodId);
				statement.registerOutParameter(6, OracleTypes.CURSOR);
				statement.executeUpdate();
				resultSet = (ResultSet) statement.getObject(7);
			}
			while (resultSet.next()) {
				return resultSet.getString(1);
			}
		} catch (SQLException e) {
			e.printStackTrace();
			logger.info("Error ocuured in generateIOCode : {}", e.getMessage());
			throw new ApplicationException("Error occurred in generateIOCode", e, Constants.DB_PROC_ERROR);
		}
		return null;
	}

}
