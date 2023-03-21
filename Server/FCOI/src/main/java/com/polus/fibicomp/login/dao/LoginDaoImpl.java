package com.polus.fibicomp.login.dao;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Restrictions;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.login.pojo.PersonLoginDetail;
import com.polus.fibicomp.login.pojo.PersonSystemNotificationMapping;
import com.polus.fibicomp.login.pojo.SystemNotification;
import com.polus.fibicomp.login.vo.LoginVO;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.PersonDTO;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.pojo.UnitAdministrator;

@SuppressWarnings("deprecation")
@Transactional
@Service(value = "loginDao")
public class LoginDaoImpl implements LoginDao {

	protected static Logger logger = LogManager.getLogger(LoginDaoImpl.class.getName());

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	public PersonDao personDao;

	private static final String PERSONID = "personId";

	public Person authenticate(String userName, String password) {
		Person person = null;
		try {
			logger.info("principalName : {}", userName);
			logger.info("password : {}", password);
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Criteria crit = session.createCriteria(Person.class);
			crit.add(Restrictions.eq("principalName", userName));
			person = (Person) crit.uniqueResult();
			logger.info("person : {}", person);
		} catch (Exception e) {
			logger.debug("Exception in authenticate: {}", e.getMessage());
		}
		return person;
	}

	public PersonDTO readPersonData(String userName) {
		PersonDTO personDTO = new PersonDTO();
		try {
			logger.info("readPersonData : {}", userName);
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Person> query = builder.createQuery(Person.class);
			Root<Person> personRoot = query.from(Person.class);
			query.where(builder.equal(personRoot.get("principalName"), userName));
			Person person = session.createQuery(query).uniqueResult();
			if (person != null && person.getStatus() != null && person.getStatus().equals("A")) {
				personDTO.setPersonID(person.getPersonId());
				personDTO.setFirstName(person.getFirstName());
				personDTO.setLastName(person.getLastName());
				personDTO.setFullName(person.getFullName());
				personDTO.setUnitNumber(person.getHomeUnit());
				personDTO.setUserName(userName);
				personDTO.setLogin(true);
			}
		} catch (Exception e) {
			logger.error("Error in method readPersonData", e);
		}
		return personDTO;
	}

	@Override
	public List<UnitAdministrator> isUnitAdmin(String personId) {
		logger.info("isUnitAdmin --- personId : {}", personId);
		List<UnitAdministrator> unitAdministrators = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<UnitAdministrator> query = builder.createQuery(UnitAdministrator.class);
		Root<UnitAdministrator> unitAdmin = query.from(UnitAdministrator.class);
		Predicate predicatePersonId = builder.equal(unitAdmin.get(PERSONID), personId);
		Predicate unitAdministratorTypeCode = builder.equal(unitAdmin.get("unitAdministratorTypeCode"), Constants.UNIT_ADMIN);
		query.where(builder.and(predicatePersonId, unitAdministratorTypeCode));
		unitAdministrators = session.createQuery(query).getResultList();
		return unitAdministrators;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public Integer changePassword(String encryptedPWD, String personId) {
		logger.info("----------- changePassword ------------");
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query<Object[]> updateQuery = session
				.createSQLQuery("UPDATE PERSON SET PASSWORD = :encryptedPWD WHERE PERSON_ID = :personId");
		updateQuery.setParameter("encryptedPWD", encryptedPWD).setString(PERSONID, personId);
		return updateQuery.executeUpdate();
	}

	@Override
	public Person getCurrentPassword(String personId) throws Exception {
		Person person = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(Person.class);
		criteria.add(Restrictions.eq(PERSONID, personId));
		person = (Person) criteria.uniqueResult();
		return person;
	}

	@Override
	public void savePersonLoginDetail(PersonLoginDetail personLoginDetail) {
		try {
			hibernateTemplate.save(personLoginDetail);
		} catch (Exception e) {
			logger.error("Exception in savePersonLoginDetail : {}", e.getMessage());
		}
	}

	@Override
	public PersonLoginDetail getPersonLoginDetailById(Integer personLoginDetailId) {
		return hibernateTemplate.get(PersonLoginDetail.class, personLoginDetailId);
	}

	@Override
	public LoginVO fetchAllPersonLoginDetails(LoginVO vo) {
		List<PersonLoginDetail> personLoginDetails = new ArrayList<>();
		Timestamp endDate = null ;
		Timestamp startDay = null;
		if (vo.getEndDate() != null) {
			endDate = Timestamp.valueOf((vo.getEndDate().toString().substring(0, 11)).concat(Constants.END_TIME));
		}
		if( vo.getStartDate() != null) {
			startDay = vo.getStartDate();
		}
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonLoginDetail> query = builder.createQuery(PersonLoginDetail.class);
		Root<PersonLoginDetail> rootPersonLoginDetail = query.from(PersonLoginDetail.class);
		Predicate predicateOne = builder.equal(rootPersonLoginDetail.get("loginStatus"), vo.getLoginStatus());
		Predicate predicateThree ;
		Predicate predicateFour = builder.equal(rootPersonLoginDetail.get("personId"), vo.getPersonId());
			if (!vo.getLoginStatus().isEmpty() && vo.getPersonId() == null) {
				predicateThree= builder.between(rootPersonLoginDetail.get("updateTimestamp"), startDay, endDate);
				query.where(builder.and(predicateOne, predicateThree));
			} else if (vo.getPersonId() != null && vo.getLoginStatus().isEmpty() ) {
				predicateThree= builder.between(rootPersonLoginDetail.get("updateTimestamp"), startDay, endDate);
				query.where(builder.and(predicateThree, predicateFour));
			} else if (!vo.getLoginStatus().isEmpty() && vo.getPersonId() != null) {
				predicateThree= builder.between(rootPersonLoginDetail.get("updateTimestamp"), startDay, endDate);
				query.where(builder.and(predicateOne, predicateThree, predicateFour));
			} else if (vo.getLoginStatus().isEmpty() && vo.getPersonId() == null) {
				predicateThree= builder.between(rootPersonLoginDetail.get("updateTimestamp"), startDay, endDate);
				query.where(builder.and(predicateThree));
			}
		if (vo.getReverse().equals("ASC")) {
			if (vo.getSortBy().equals("personId")) {
				query.orderBy(builder.asc(rootPersonLoginDetail.get("personId")));
			} else if (vo.getSortBy().equals("person.fullName")) {
				query.orderBy(builder.asc(rootPersonLoginDetail.get("fullName")));
			} else if (vo.getSortBy().equals("dateAndTime") || vo.getSortBy().equals("")) {
				query.orderBy(builder.asc(rootPersonLoginDetail.get("updateTimestamp")));
			}
		} else {
			 if (vo.getSortBy().equals("personId")) {
				query.orderBy(builder.desc(rootPersonLoginDetail.get("personId")));
			} else if (vo.getSortBy().equals("person.fullName")) {
				query.orderBy(builder.desc(rootPersonLoginDetail.get("fullName")));
			} else if (vo.getSortBy().equals("dateAndTime") || vo.getSortBy().equals("")) {
				query.orderBy(builder.desc(rootPersonLoginDetail.get("updateTimestamp")));
			}
		}
		Integer count = (session.createQuery(query).getResultList()).size();
		vo.setUserActivityCount(count);
		if (Boolean.TRUE.equals(vo.getIsDownload())) {
			personLoginDetails = session.createQuery(query).getResultList();
		} else {
			personLoginDetails = session.createQuery(query).setFirstResult((vo.getCurrentPage() - 1) * vo.getItemsPerPage()).setMaxResults(vo.getItemsPerPage()).getResultList();
		}
		vo.setPersonLoginDetails(personLoginDetails);
		return vo;
	}

	@Override
	public Boolean isExternalUser(String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Person> query = builder.createQuery(Person.class);
		Root<Person> person = query.from(Person.class);
		Predicate predicateOne = builder.equal(person.get("personId"), personId);
		Predicate predicateTwo = builder.isTrue(person.get("isExternalUser"));
		query.where(builder.and(predicateOne, predicateTwo));
		List<Person> persons = session.createQuery(query).getResultList();
		if (persons != null && !persons.isEmpty()) {
			return true;
		} else {
			return false;
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List<Unit> getUnitsListByPersonIdAndRights(String personId, List<String> systemRights) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT T1.unitNumber, T1.unitName From Unit T1 where T1.unitNumber IN (SELECT T2.personRoleRTAttributes.unitNumber from PersonRoleRT T2 where T2.personRoleRTAttributes.personId =:personId and T2.personRoleRTAttributes.rightName IN (:rightName))";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("personId", personId);
		query.setParameter("rightName", systemRights);
		if (query.getResultList() != null && !(query.getResultList()).isEmpty()) {
			return query.getResultList();
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public LoginVO getPersonLoginDetails(LoginVO vo, List<String> unitList) {
		List<PersonLoginDetail> personLoginDetails = new ArrayList<>();
		Timestamp endDate = null ;
		Timestamp startDay = null;
		if (vo.getEndDate() != null) {
			endDate = Timestamp.valueOf((vo.getEndDate().toString().substring(0, 11)).concat(Constants.END_TIME));
		}
		if( vo.getStartDate() != null) {
			startDay = vo.getStartDate();
		}
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select T1 from PersonLoginDetail T1 ");
		hqlQuery.append(" inner join Person T2 ON T2.personId = T1.personId ");
		hqlQuery.append(" left join Unit T3 ON T3.unitNumber = T2.homeUnit where ");
		if (!vo.getLoginStatus().isEmpty() && vo.getPersonId() == null) {
			hqlQuery.append(" T1.loginStatus = (:loginStatus) and T1.updateTimestamp BETWEEN  :startDay AND :endDate");
		} else if (vo.getPersonId() != null && vo.getLoginStatus().isEmpty()) {
			hqlQuery.append(" T1.personId=:personId and T1.updateTimestamp BETWEEN  :startDay AND :endDate ");
		} else if (!vo.getLoginStatus().isEmpty() && vo.getPersonId() != null) {
			hqlQuery.append(
					" T1.personId=:personId and T1.loginStatus = (:loginStatus) and T1.updateTimestamp BETWEEN  :startDay AND :endDate ");
		} else if (vo.getLoginStatus().isEmpty() && vo.getPersonId() == null) {
			hqlQuery.append(" T1.updateTimestamp BETWEEN  :startDay AND :endDate ");
		}
		if (unitList != null && !unitList.isEmpty()) {
			hqlQuery.append(" and (T1.personId in (select personId from Person  where homeUnit in (:homeUnit)))");
		} 
		if (vo.getReverse().equals("ASC")) {
			if (vo.getSortBy().equals("personId")) {
				hqlQuery.append(" order by T1.personId asc ");
			} else if (vo.getSortBy().equals("person.fullName")) {
				hqlQuery.append(" order by T2.fullName asc ");
			} else if (vo.getSortBy().equals("dateAndTime") || vo.getSortBy().equals("")) {
				hqlQuery.append(" order by T1.updateTimestamp asc ");
			} else if (vo.getSortBy().equals("loginStatus")) {
				hqlQuery.append(" order by T1.loginStatus asc ");
			} else if (vo.getSortBy().equals("unit.unitName")) {
				hqlQuery.append(" order by T3.unitName asc ");
			}
		} else {
			 if (vo.getSortBy().equals("personId")) {
				 hqlQuery.append(" order by T1.personId desc ");
			} else if (vo.getSortBy().equals("person.fullName")) {
				hqlQuery.append(" order by T2.fullName desc ");
			} else if (vo.getSortBy().equals("dateAndTime") || vo.getSortBy().equals("")) {
				hqlQuery.append(" order by T1.updateTimestamp desc ");
			} else if (vo.getSortBy().equals("loginStatus")) {
				hqlQuery.append(" order by T1.loginStatus desc ");
			} else if (vo.getSortBy().equals("unit.unitName")) {
				hqlQuery.append(" order by  T3.unitName desc ");
			}
		}
		Query findPersonDetail = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		if (unitList != null && !unitList.isEmpty()) {
			findPersonDetail.setParameter("homeUnit", unitList);
		}
		if(vo.getLoginStatus() != null && !vo.getLoginStatus().equals("")) {
			findPersonDetail.setParameter("loginStatus", vo.getLoginStatus());
		}
		if(vo.getPersonId() != null && ! vo.getPersonId().equals("")) {
			findPersonDetail.setParameter("personId", vo.getPersonId());
		}
		findPersonDetail.setParameter("startDay", startDay);
		findPersonDetail.setParameter("endDate", endDate);
		Integer count = findPersonDetail.getResultList().size();
		vo.setUserActivityCount(count);
		if (Boolean.TRUE.equals(vo.getIsDownload())) {
			personLoginDetails = findPersonDetail.getResultList();
		} else {
			personLoginDetails = findPersonDetail.setFirstResult((vo.getCurrentPage() - 1) * vo.getItemsPerPage()).setMaxResults(vo.getItemsPerPage()).getResultList();
		}
		vo.setPersonLoginDetails(personLoginDetails);
		return vo;		
	}

	@SuppressWarnings({"rawtypes" })
	@Override
	public PersonLoginDetail getRecentPersonLoginDetailByUserName(String userName) {
		PersonLoginDetail loginDetails = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT t1 FROM PersonLoginDetail t1 WHERE t1.loginDetailId = (SELECT max(t2.loginDetailId) FROM PersonLoginDetail t2 WHERE t2.personId=(SELECT t3.personId FROM Person t3 WHERE t3.principalName =:userName) AND t2.loginStatus=:status)";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("userName", userName);
		query.setParameter("status", "IN");
		loginDetails = (PersonLoginDetail) query.uniqueResult();
		return loginDetails;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<SystemNotification> fetchSystemAllNotification(Timestamp currentDate) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" From SystemNotification t1 where :currentDate between t1.publishedStartDate and t1.publishedEndDate order by t1.sortOrder desc");
		javax.persistence.Query findPerson = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		findPerson.setParameter("currentDate", currentDate);
		return findPerson.getResultList();
	}

	@Override
	public PersonSystemNotificationMapping getPersonSystemNotificationByNotificationId(Integer systemNotificationId,  String personId) {
		try {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonSystemNotificationMapping> query = builder.createQuery(PersonSystemNotificationMapping.class);
		Root<PersonSystemNotificationMapping> rootSystemNotification = query.from(PersonSystemNotificationMapping.class);
		Predicate predicateSystemNotificationId = builder.equal(rootSystemNotification.get("systemNotificationId"),systemNotificationId);
		Predicate predicatePersonId = builder.equal(rootSystemNotification.get("personId"),personId);
		query.where(builder.and(predicateSystemNotificationId,predicatePersonId));
		return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			logger.error("error in getPersonSystemNotificationByNotificationId {}",e.getMessage());
			return null;
		}
	}

	@Override
	public void deletePersonSystemNotificationMapping(Integer systemNotificationId, String personId) {
		javax.persistence.Query query = null;
		query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from PersonSystemNotificationMapping c1 where c1.systemNotificationId =:systemNotificationId and c1.personId =:personId");
		query.setParameter("personId", personId);
		query.setParameter("systemNotificationId", systemNotificationId);
		query.executeUpdate();
	}

	@Override
	public void saveOrUpdatePersonSystemNotificationMapping(PersonSystemNotificationMapping personSystemNotificationMapping) {
		hibernateTemplate.saveOrUpdate(personSystemNotificationMapping);	
	}

}
