package com.polus.fibicomp.integratefile.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;

import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.integratefile.pojo.PersonFeed;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "integrationFileDao")
public class IntegrationFileDaoImpl implements IntegrationFileDao {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Override
	public PersonFeed savePersonFeedDetails(PersonFeed personFeed) {
		try {
			hibernateTemplate.saveOrUpdate(personFeed);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return personFeed;
	}

	@Override
	public boolean savePersonFeedDetailToPerson() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call PERSON_FEED ()}");
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call PERSON_FEED ()}");
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				if ((resultSet.getString("RETURN_VALUE")).equals("1")) {
					return true;
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return false;
	}

	public void deleteAllPersonFeed() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<PersonFeed> query = builder.createCriteriaDelete(PersonFeed.class);
		query.from(PersonFeed.class);
		session.createQuery(query).executeUpdate();
	}

}
