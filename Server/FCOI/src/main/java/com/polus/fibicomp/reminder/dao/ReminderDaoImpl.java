package com.polus.fibicomp.reminder.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
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

import com.polus.fibicomp.reminder.pojo.ReminderNotification;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "remainderDao")
public class ReminderDaoImpl implements ReminderDao {

	protected static Logger logger = LogManager.getLogger(ReminderDaoImpl.class.getName());

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<ReminderNotification> fetchAllActiveReminders() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ReminderNotification> query = builder.createQuery(ReminderNotification.class);
		Root<ReminderNotification> parameter = query.from(ReminderNotification.class);
		query.where(builder.equal(parameter.get("isActive"), "Y"));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ResultSet getReminderData(Integer days, String procedureName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call " +procedureName+"(?)}");
				statement.setInt(1, days);
				statement.execute();
				resultSet= statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call " + procedureName + "(?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, days);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
		} catch (SQLException e) {
			logger.error("error occured in getRemainderData : {}", e.getMessage());
		}
		return resultSet;
	}

}
