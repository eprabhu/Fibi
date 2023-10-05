package com.polus.fibicomp.opa.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.SQLException;

import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "opaDaoImpl")
public class OPADaoImpl implements OPADao {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public boolean isOpaDisclosureRequired(String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			String functionName = "FN_OPA_DISCLOSURE_REQUIRED";
			String functionCall = "{ ? = call " + functionName + "(?) }";
			statement = connection.prepareCall(functionCall);
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setString(2, personId);
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

	@Override
	public void createOpaDisclosure(String personId, String homeUnit) {
//		procedure call
	}

}
