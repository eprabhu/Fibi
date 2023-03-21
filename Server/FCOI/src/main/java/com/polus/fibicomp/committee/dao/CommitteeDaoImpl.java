package com.polus.fibicomp.committee.dao;

import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.transform.Transformers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.Rolodex;
import com.polus.fibicomp.pojo.Unit;

@Transactional
@Service(value = "committeeDao")
public class CommitteeDaoImpl implements CommitteeDao {

	protected static Logger logger = LogManager.getLogger(CommitteeDaoImpl.class.getName());

	private static final String DESCRIPTION = "description";

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@SuppressWarnings("deprecation")
	@Override
	public List<Unit> fetchAllHomeUnits() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(Unit.class);
		ProjectionList projList = Projections.projectionList();
		projList.add(Projections.property("unitNumber"), "unitNumber");
		projList.add(Projections.property("unitName"), "unitName");
		criteria.setProjection(projList).setResultTransformer(Transformers.aliasToBean(Unit.class));
		criteria.add(Restrictions.eq("active", true));
		criteria.addOrder(Order.asc("unitName"));
		@SuppressWarnings("unchecked")
		List<Unit> units = criteria.list();
		return units;
	}

	@Override
	public Date getCurrentDate() {
		Calendar c = Calendar.getInstance();
		c.setTime(new Date());
		return c.getTime();
	}

	@Override
	public Timestamp getCurrentTimestamp() {
		return new Timestamp(this.getCurrentDate().getTime());
	}

	@Override
	public String convertObjectToJSON(Object object) {
		String response = "";
		ObjectMapper mapper = new ObjectMapper();
		try {
			mapper.setVisibility(PropertyAccessor.FIELD, Visibility.ANY);
			response = mapper.writeValueAsString(object);
		} catch (Exception e) {
			e.printStackTrace();
		}

		return response;
	}

	@SuppressWarnings("deprecation")
	@Override
	public List<Person> getAllEmployees() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(Person.class);
		ProjectionList projList = Projections.projectionList();
		projList.add(Projections.property("personId"), "personId");
		projList.add(Projections.property("fullName"), "fullName");
		criteria.setProjection(projList).setResultTransformer(Transformers.aliasToBean(Person.class));
		criteria.addOrder(Order.asc("fullName"));
		@SuppressWarnings("unchecked")
		List<Person> employeesList = criteria.list();
		return employeesList;
	}

	@SuppressWarnings("deprecation")
	@Override
	public List<Rolodex> getAllNonEmployees() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(Rolodex.class);
		criteria.add(Restrictions.eq("active", true));
		ProjectionList projList = Projections.projectionList();
		projList.add(Projections.property("rolodexId"), "rolodexId");
		projList.add(Projections.property("firstName"), "firstName");
		projList.add(Projections.property("lastName"), "lastName");
		projList.add(Projections.property("middleName"), "middleName");
		projList.add(Projections.property("prefix"), "prefix");
		criteria.setProjection(projList).setResultTransformer(Transformers.aliasToBean(Rolodex.class));
		criteria.addOrder(Order.asc("lastName"));
		@SuppressWarnings("unchecked")
		List<Rolodex> nonEmployeesList = criteria.list();
		return nonEmployeesList;
	}

	@Override
	public Person getPersonDetailsById(String personId) {
		return hibernateTemplate.get(Person.class, personId);
	}

	@Override
	public Rolodex getRolodexById(Integer rolodexId) {
		return hibernateTemplate.get(Rolodex.class, rolodexId);
	}

}
