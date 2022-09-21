package com.polus.fibicomp.award.awardworkflow.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.budget.pojo.AwardWorkflowStatus;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "awardWorkflowDao")
public class AwardWorkflowDaoImpl implements AwardWorkflowDao {

	protected static Logger logger = LogManager.getLogger(AwardWorkflowDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Override
	public AwardWorkflowStatus getAwardWorkFlowStatusByCode(String statusCode) {
		return hibernateTemplate.get(AwardWorkflowStatus.class, statusCode);
	}

	@Override
	public Integer addAlternativeApprover(AwardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer result = 0;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call INSERT_WORKFLOW_ALTER_APPROVER(?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setInt(1, vo.getWorkFlowId());
				statement.setInt(2, vo.getMapId());
				statement.setInt(3, vo.getMapNumber());
				statement.setInt(4, vo.getApprovalStopNumber());
				statement.setInt(5, vo.getApproverNumber());
				statement.setString(6, vo.getApproverPersonId());
				statement.setString(7, vo.getApprovalStatus());
				statement.setString(8, vo.getUpdateUser());
				statement.setString(9, vo.getApproverFlag());
				statement.setString(10, vo.getMapName());
				statement.setString(11, vo.getMapDescription());
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "INSERT_WORKFLOW_ALTER_APPROVER";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setInt(1, vo.getWorkFlowId());
				statement.setInt(2, vo.getMapId());
				statement.setInt(3, vo.getMapNumber());
				statement.setInt(4, vo.getApprovalStopNumber());
				statement.setInt(5, vo.getApproverNumber());
				statement.setString(6, vo.getApproverPersonId());
				statement.setString(7, vo.getApprovalStatus());
				statement.setString(8, vo.getUpdateUser());
				statement.setString(9, vo.getApproverFlag());
				statement.setString(10, vo.getMapName());
				statement.setString(11, vo.getMapDescription());
				statement.registerOutParameter(12, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(12);
			}
			while (resultSet.next()) {
				result = Integer.parseInt(resultSet.getString(1));
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return result;
	}

}
