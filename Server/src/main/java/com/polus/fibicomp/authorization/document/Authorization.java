package com.polus.fibicomp.authorization.document;

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
@Service(value = "userDocumentAuthorization")
public class Authorization implements UserDocumentAuthorization{

	@Autowired
	private HibernateTemplate hibernateTemplate;
	
	public boolean checkAuthorized(Integer moduleCode,String moduleItemKey,String loggedInPerson) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			String functionName = "FN_PERSON_HAS_AUTHORIZATION";
			String functionCall = "{ ? = call " + functionName + "(?,?,?) }";
			statement = connection.prepareCall(functionCall);
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setInt(2, moduleCode);
			statement.setString(3, moduleItemKey);
			statement.setString(4, loggedInPerson);
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
	public boolean isAuthorized(Integer moduleCode, String moduleItemKey, String loggedInPerson) {
		return checkAuthorized(moduleCode, moduleItemKey, loggedInPerson);
	}
	
}
