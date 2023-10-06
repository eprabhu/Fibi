package com.polus.fibicomp.opa.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.security.AuthenticatedUser;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "opaDaoImpl")
public class OPADaoImpl implements OPADao {

	protected static Logger logger = LogManager.getLogger(OPADaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public boolean isOpaDisclosureRequired(String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		try {
			CallableStatement statement = connection.prepareCall("{ ? = call FN_CAN_CREATE_OPA_DISCLOSURE (?) }");
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setString(2, personId);
			statement.execute();
			int result = statement.getInt(1);
			return result == 1;
		} catch (SQLException e) {
			return Boolean.FALSE;
		}
	}

	@Override
	public Integer createOpaDisclosure(String personId, String homeUnit) {
		Session session = hibernateTemplate.getSessionFactory().openSession();
		try {
			SessionImpl sessionImpl = (SessionImpl) session;
			Transaction transaction = session.beginTransaction();
			Connection connection = sessionImpl.connection();
			CallableStatement statement = connection.prepareCall("{call INSERT_OPA_DISCLOSURE_DETAILS(?,?,?)}");
			statement.setString(1, personId);
			statement.setString(2, homeUnit);
			statement.setString(3, AuthenticatedUser.getLoginUserName());
			statement.execute();
			transaction.commit();
			statement.getMoreResults();
			try (ResultSet resultSet = statement.getResultSet()) {
				while (resultSet.next()) {
					return resultSet.getInt("LI_OPA_DISCLOSURE_ID");
				}
			}
		} catch (Exception e) {
			logger.error("Exception in createOpaDisclosure {}", e.getMessage());
		} finally {
			session.close();
		}
		return null;
	}

}
