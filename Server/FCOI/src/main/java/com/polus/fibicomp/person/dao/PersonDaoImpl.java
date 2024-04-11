package com.polus.fibicomp.person.dao;

import java.io.File;
import java.io.FileOutputStream;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.commons.collections4.ListUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.transform.Transformers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.person.pojo.DegreeType;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.person.pojo.PersonDegree;
import com.polus.fibicomp.person.pojo.PersonRoleRT;
import com.polus.fibicomp.person.vo.PersonSearchResult;
import com.polus.fibicomp.person.vo.PersonVO;
import com.polus.fibicomp.persontraining.pojo.PersonTraining;
import com.polus.fibicomp.roles.pojo.PersonRoles;

@Transactional
@Service(value = "personDao")
public class PersonDaoImpl implements PersonDao {

	protected static Logger logger = LogManager.getLogger(PersonDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	public CommonDao commonDao;
	
	@Value("${system.timezone}")
	private String timezone;
	
	@Value("${log.filepath}")
	private String filePath;

	@Override
	public List<PersonRoleRT> fetchCreateProposalPersonRole(String personId, String rightName) {
		List<PersonRoleRT> personRoles = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonRoleRT> query = builder.createQuery(PersonRoleRT.class);
		Root<PersonRoleRT> rootPersonRole = query.from(PersonRoleRT.class);
		Predicate predicate1 = builder.equal(rootPersonRole.get("personRoleRTAttributes").get("personId"), personId);
		Predicate predicate2 = builder.equal(rootPersonRole.get("personRoleRTAttributes").get("rightName"), rightName);
		query.where(builder.and(predicate1, predicate2));
		query.distinct(true);
		personRoles = session.createQuery(query).getResultList();
		logger.debug(personRoles.size());
		return personRoles;
	}

	@Override
	public boolean fetchSuperUserPersonRole(String personId, String rightName) {
		boolean isSuperUser = false;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonRoleRT> query = builder.createQuery(PersonRoleRT.class);
		Root<PersonRoleRT> rootPersonRole = query.from(PersonRoleRT.class);
		Predicate predicate1 = builder.equal(rootPersonRole.get("personRoleRTAttributes").get("personId"), personId);
		Predicate predicate2 = builder.equal(rootPersonRole.get("personRoleRTAttributes").get("rightName"), rightName);
		query.where(builder.and(predicate1, predicate2));
		List<PersonRoleRT> personRoles = session.createQuery(query).getResultList();
		if (personRoles != null && !personRoles.isEmpty()) {
			isSuperUser = true;
		}
		return isSuperUser;
	}

	@Override
	public boolean isPersonHasPermissionInAnyDepartment(String personId, String permissionName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonRoleRT> query = builder.createQuery(PersonRoleRT.class);
		Root<PersonRoleRT> root = query.from(PersonRoleRT.class);
		Predicate predicate1 = builder.equal(root.get("personRoleRTAttributes").get("personId"), personId);
		Predicate predicate2 = builder.equal(root.get("personRoleRTAttributes").get("rightName"), permissionName);
		query.where(builder.and(predicate1, predicate2));
		boolean noResults = session.createQuery(query).list().isEmpty();
		return noResults == true ? false : true;

	}

	@Override
	public boolean isPersonHasPermission(String personId, String permissionName, String unitNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonRoleRT> query = builder.createQuery(PersonRoleRT.class);
		Root<PersonRoleRT> root = query.from(PersonRoleRT.class);
		Predicate predicate1 = builder.equal(root.get("personRoleRTAttributes").get("personId"), personId);
		Predicate predicate2 = builder.equal(root.get("personRoleRTAttributes").get("rightName"), permissionName);
		Predicate predicate3 = builder.equal(root.get("personRoleRTAttributes").get("unitNumber"), unitNumber);
		query.where(builder.and(predicate1, predicate2, predicate3));
		boolean noResults = session.createQuery(query).list().isEmpty();
		return noResults == true ? false : true;
	}

	@Override
	public Person getPersonDetailById(String personId) {
		return hibernateTemplate.get(Person.class, personId);
	}

	@Override
	public List<PersonSearchResult> findPerson(String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		final String likeCriteria = "%" + searchString.toUpperCase() + "%";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<PersonSearchResult> query = session.createQuery(
				"SELECT NEW com.polus.fibicomp.person.vo.PersonSearchResult(t.personId, t.fullName) " + "FROM Person t "
						+ "WHERE UPPER(t.personId) like :likeCriteria OR UPPER(t.fullName) like :likeCriteria");
		query.setParameter("likeCriteria", likeCriteria);
		return ListUtils.emptyIfNull(query.setMaxResults(25).list());
	}

	@Override
	public Person saveOrUpdatePerson(Person person) {
		try {
			hibernateTemplate.saveOrUpdate(person);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return person;
	}

	@SuppressWarnings("deprecation")
	@Override
	public PersonVO getAllPersons(PersonVO vo) {
		Integer pageNumber = vo.getPageNumber();
		Integer currentPage = vo.getCurrentPage();
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		String property3 = vo.getProperty3();
		String property4 = vo.getProperty4();
		String property5 = vo.getProperty5();
		String property6 = vo.getProperty6();
		String property7 = vo.getProperty7();
		String property8 = vo.getProperty8();
		String property9 = vo.getProperty9();
		String property10 = vo.getProperty10();
		String property11 = vo.getProperty11();
		List<String> property12 = vo.getProperty12();
		Map<String, String> sort = vo.getSort();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria countCriteria = session.createCriteria(Person.class);
		Conjunction and = Restrictions.conjunction();
		Criteria criteria = session.createCriteria(Person.class);
		countCriteria.createAlias("unit", "unit", Criteria.LEFT_JOIN);
		criteria.createAlias("unit", "unit", Criteria.LEFT_JOIN);
		if (sort.isEmpty()) {
			criteria.addOrder(Order.desc("updateTimestamp")).addOrder(Order.desc("personId"));
		} else {
			for (Map.Entry<String, String> mapElement : sort.entrySet()) {
				if (mapElement.getValue().equalsIgnoreCase("desc")) {
					criteria.addOrder(Order.desc(mapElement.getKey()));
				} else {
					criteria.addOrder(Order.asc(mapElement.getKey()));
				}
			}
		}
		if (property1 != null && !property1.isEmpty()) {
			and.add(Restrictions.like("firstName", "%" + property1 + "%").ignoreCase());
		}
		if (property2 != null && !property2.isEmpty()) {
			and.add(Restrictions.like("lastName", "%" + property2 + "%").ignoreCase());
		}
		if (property3 != null && !property3.isEmpty()) {
			and.add(Restrictions.like("middleName", "%" + property3 + "%").ignoreCase());
		}
		if (property4 != null && !property4.isEmpty()) {
			and.add(Restrictions.like("principalName", "%" + property4 + "%").ignoreCase());
		}
		if (property5 != null && !property5.isEmpty()) {
			and.add(Restrictions.like("emailAddress", "%" + property5 + "%").ignoreCase());
		}
		if (property6 != null && !property6.isEmpty()) {
			and.add(Restrictions.like("primaryTitle", "%" + property6 + "%").ignoreCase());
		}
		if (property7 != null && !property7.isEmpty()) {
			and.add(Restrictions.like("directoryTitle", "%" + property7 + "%").ignoreCase());
		}
		if (property8 != null && !property8.isEmpty()) {
			and.add(Restrictions.like("homeUnit", "%" + property8 + "%").ignoreCase());
		}
		if (property9 != null && !property9.isEmpty()) {
			and.add(Restrictions.like("state", "%" + property9 + "%").ignoreCase());
		}
		if (property10 != null && !property10.isEmpty()) {
			and.add(Restrictions.like("fullName", "%" + property10 + "%").ignoreCase());
		}
		if (property11 != null && !property11.isEmpty()) {
			and.add(Restrictions.like("personId", "%" + property11 + "%").ignoreCase());
		}
		if (property12 != null && !property12.isEmpty()) {
			and.add(Restrictions.in("status", property12));
		}
		criteria.add(and);
		countCriteria.add(and);
		ProjectionList distinctProjectionList = Projections.projectionList();
		ProjectionList projectionList = Projections.projectionList();
		projectionList.add(Projections.distinct(distinctProjectionList.add(Projections.property("personId"), "personId")));
		projectionList.add(Projections.property("lastName"), "lastName");
		projectionList.add(Projections.property("firstName"), "firstName");
		projectionList.add(Projections.property("principalName"), "principalName");
		projectionList.add(Projections.property("emailAddress"), "emailAddress");
		projectionList.add(Projections.property("unit"), "unit");
		projectionList.add(Projections.property("primaryTitle"), "primaryTitle");
		projectionList.add(Projections.property("mobileNumber"), "mobileNumber");
		projectionList.add(Projections.property("fullName"), "fullName");
		projectionList.add(Projections.property("personId"), "personId");
		projectionList.add(Projections.property("status"), "status");
		criteria.setProjection(projectionList);
		criteria.setResultTransformer(Transformers.aliasToBean(Person.class)); 
		countCriteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
		Long personCount = (Long) countCriteria.setProjection(Projections.rowCount()).uniqueResult();
		logger.info("personCount : " + personCount);
		int count = pageNumber * (currentPage - 1);
		criteria.setFirstResult(count);
		criteria.setMaxResults(pageNumber);
		@SuppressWarnings("unchecked")
		List<Person> persons = criteria.list();
		vo.setPersons(persons);
		vo.setTotalPersons(personCount.intValue());
		return vo;
	}

	@Override
	public void deletePerson(Person person) {
		hibernateTemplate.delete(person);
	}

	@Override
	public PersonVO getAllTrainings(PersonVO vo) {
		List<PersonTraining> trainings = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();		
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonTraining> query = builder.createQuery(PersonTraining.class);
		Root<PersonTraining> personTraining = query.from(PersonTraining.class);
		Predicate predicate1 = builder.equal(personTraining.get("personId"), vo.getPersonId());
		query.where(builder.and(predicate1));
		trainings = session.createQuery(query).getResultList();
		vo.setTrainings(trainings);
		return vo;
	}

	@Override
	public boolean checkUniquePrincipalName(String principalName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT COUNT(*) FROM Person P WHERE P.principalName = :principalName";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("principalName", principalName);
		Long count = (Long) query.getSingleResult();
		if (count > 0) {
			return true;
		}
		return false;
	}

	@Override
	public String getUserFullNameByUserName(String userName) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT P.fullName FROM Person P WHERE P.principalName = :principalName";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("principalName", userName);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			return null;		
		}
	}

	@Override
	public String getPersonIdByUserName(String userName) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT P.personId FROM Person P WHERE P.principalName = :principalName";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("principalName", userName);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			return null;		
		}
	}

	@Override
	public String getPersonFullNameByPersonId(String personId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT P.fullName FROM Person P WHERE P.personId = :personId";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("personId", personId);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<PersonRoles> getPersonRoleByUnitNumberAndRoleId(String unitNumber, Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonRoles> query = builder.createQuery(PersonRoles.class);
		Root<PersonRoles> root = query.from(PersonRoles.class);
		Predicate predicate1 = builder.equal(root.get("unitNumber"), unitNumber);
		Predicate predicate2 = builder.equal(root.get("roleId"), roleId);
		query.where(builder.and(predicate1, predicate2));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<PersonRoleRT> fetchPersonRoleRTByRightNameAndUnitNumber(String unitNumber, String rightName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonRoleRT> query = builder.createQuery(PersonRoleRT.class);
		Root<PersonRoleRT> rootPersonRole = query.from(PersonRoleRT.class);
		Predicate predicate1 = builder.equal(rootPersonRole.get("personRoleRTAttributes").get("unitNumber"), unitNumber);
		Predicate predicate2 = builder.equal(rootPersonRole.get("personRoleRTAttributes").get("rightName"), rightName);
		query.where(builder.and(predicate1, predicate2));
		query.distinct(true);
        return session.createQuery(query).getResultList();
	}

	@Override
	public List<PersonRoles> getPersonRolesByRoleId(Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonRoles> query = builder.createQuery(PersonRoles.class);
		Root<PersonRoles> root = query.from(PersonRoles.class);
		Predicate predicate1 = builder.equal(root.get("roleId"), roleId);
		query.where(builder.and(predicate1));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Person getPersonDetailByPrincipalName(String principalName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM Person P WHERE P.principalName = :principalName";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("principalName", principalName);
		return (Person) query.getSingleResult();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public Set<String> fetchPersonIdByParams(String unitNumber, List<Integer> roleIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Set<String> recipientSet = new HashSet();
 		String personId = "SELECT T.personId FROM PersonRoles T WHERE T.unitNumber = :unitNumber and T.roleId in (:roleId)";
		Query query = session.createQuery(personId);
		query.setParameter("unitNumber", unitNumber);
		query.setParameter("roleId", roleIds);
		if (query.getResultList() != null && !query.getResultList().isEmpty()) {
			recipientSet.addAll(query.getResultList());
		}	
        return recipientSet;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public Set<String> fetchPersonIdByUnitIdAndRoleId(String unitNumber, Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Set<String> recipientSet = new HashSet();
 		String hqlQuery = "SELECT T.personId FROM PersonRoles T WHERE T.unitNumber = :unitNumber and T.roleId = :roleId";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("unitNumber", unitNumber);
		query.setParameter("roleId",roleId);
		if (query.getResultList() != null && !query.getResultList().isEmpty()) {
			recipientSet.addAll(query.getResultList());
		}	
        return recipientSet;
	}
	
	@Override
	public String savePersonFromFeed(Person person) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			if(session.getTransaction().isActive()==false) {
				session.getTransaction().begin();
			}
			hibernateTemplate.saveOrUpdate(person);
			session.getTransaction().commit();
		} catch (Exception e) {
			//e.printStackTrace();
			logger.info("Exception in saving person from feed");
			String personType;
			if(person.getIsGraduateStudentStaff()==true) {
				personType = "student";
			}else {
				personType = "staff";
			}
			logDetailsInFile(personType, "Exception occured while insertion of person from feed with PersonId: " + person.getPersonId()+ "<br/>" + e + "<br/>" + " at : ");
			return "Failure";
		}
		return "Success";
	}

	public void logDetailsInFile(String personType, String fileContent) {
		String fileName;
		try {
			if(personType.equalsIgnoreCase("staff")) {
			 fileName = "Staff_Feed_Logs";
			}else {
			 fileName = "Student_Feed_Logs";
			}
			String date = convertDateFormatBasedOnTimeZone(commonDao.getCurrentTimestamp().getTime(), Constants.LOG_DATE_FORMAT);
			DateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
			Date currentDate = new Date(commonDao.getCurrentTimestamp().getTime());
//			dateFormat.setTimeZone(TimeZone.getTimeZone(timezone));
			String fileNameWithPath = filePath + File.separator + fileName + "_" + date + ".log";
			File file = new File(fileNameWithPath);
			FileOutputStream fileOutputStream = null;
			if (file.exists()) {
				fileOutputStream = new FileOutputStream(fileNameWithPath, true);
			} else {
				fileOutputStream = new FileOutputStream(fileNameWithPath);
			}
			fileContent = fileContent + " - " + dateFormat.format(currentDate) + "\n";
			fileOutputStream.write(fileContent.getBytes());
			fileOutputStream.close();
		} catch (Exception e) {
			logger.error("Exception in method logDetailsInFile : {} ", e.getMessage());
		}
	}
	public String convertDateFormatBasedOnTimeZone(Long dateValue,String dateFormat) {
		Date date = new Date(dateValue);
		String formattedDate = new SimpleDateFormat(dateFormat).format(commonDao.adjustTimezone(date));
		return formattedDate;
	}

	@Override
	public Manpower getManpowerDetailsbyPersonId(String manpowerPersonId) {
		try {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Manpower> query = builder.createQuery(Manpower.class);
		Root<Manpower> rootAwardManpower = query.from(Manpower.class);
		query.where(builder.and(builder.equal(rootAwardManpower.get("manpowerPersonId"), manpowerPersonId)));
		return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			logger.error("Exception in method getManpowerDetailsbyPersonId : {} ", e.getMessage());
			return new Manpower();
		}
	}

	@Override
	public List<Person> getInactivePersons() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Person> query = builder.createQuery(Person.class);
		Root<Person> rootPerson = query.from(Person.class);
		Predicate predicateStatus = builder.equal(rootPerson.get("status"), "I");
		Predicate predicateIsGraduateStudentStaff = builder.equal(rootPerson.get("isGraduateStudentStaff"), true);
		query.where(builder.and(predicateStatus, predicateIsGraduateStudentStaff));
		return session.createQuery(query).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getInactivePersonsFromManpower(Set<String> personIds, Timestamp executionStartTime) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select distinct T2.awardId, T3.personId, T3.fullName from Award T1 inner join AwardManpower T2 on T2.awardId = T1.awardId");
		hqlQuery.append("  inner join AwardManpowerResource T3 on T3.awardManpowerId = T2.awardManpowerId");
		hqlQuery.append("  where  T3.personId IN (:personId) and T1.awardSequenceStatus = 'ACTIVE' and :executionStartTime between T3.chargeStartDate and T3.chargeEndDate");
		org.hibernate.query.Query<Object[]> findPersons = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		findPersons.setParameter("personId", personIds);
		findPersons.setParameter("executionStartTime", executionStartTime);
		return findPersons.list();
	}

	@Override
	public String getPersonPassword(String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String password = "SELECT password FROM  Person where personId =:personId";
		Query query = session.createQuery(password);
		query.setParameter("personId", personId);
	    return (String) query.getSingleResult();
	}

	@Override
	public boolean checkForPersonInactive(String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT COUNT(*) FROM Person P WHERE P.personId = :personId and P.status = 'I'";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("personId", personId);
		return (long)query.getSingleResult() > 0;
    }
  
	@SuppressWarnings("unchecked")
	@Override
	public List<PersonRoles> getPersonRolesByRoleAndRightId(Integer roleId, Integer rightId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT Distinct T1 FROM PersonRoles T1 INNER JOIN RoleRights T2 ON T1.roleId = T2.roleId\n" + 
				"    INNER JOIN Rights T3 ON T2.rightId = T3.rightId \n" + 
				"    WHERE T3.rightId = :rightId OR T2.roleId = :roleId";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("rightId", rightId);
		query.setParameter("roleId", roleId);
		return query.getResultList();
	}

	@Override
	public String getPersonUnitNumberByPersonId(String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String password = "SELECT homeUnit FROM  Person where personId =:personId";
		Query query = session.createQuery(password);
		query.setParameter("personId", personId);
	    return (String) query.getSingleResult();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> getGroupAdminPersonIdsByRightName(String rightName, Integer adminGroupId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("select t1.personId from PersonRoles t1 left join RoleRights t2 on t2.roleId = t1.roleId ");
		hqlQuery.append("left join Rights t3 on t3.rightId = t2.rightId ");
		hqlQuery.append("left join AdminGroup t4 on t4.roleId = t2.roleId ");
		hqlQuery.append("where t3.rightName = :rightName and t4.adminGroupId = :adminGroupId");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());		
		query.setParameter("rightName",rightName);
		query.setParameter("adminGroupId",adminGroupId);
		return query.getResultList();
	}
	
	@Override
	public List<PersonDegree> getAllPersonDegree(String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();		
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonDegree> query = builder.createQuery(PersonDegree.class);
		Root<PersonDegree> personTraining = query.from(PersonDegree.class);
		Predicate predicate1 = builder.equal(personTraining.get("personId"), personId);
		query.where(builder.and(predicate1));
		return (session.createQuery(query).getResultList());
	}
	
	@Override
	public PersonDegree savePersonDegree(PersonDegree personDegree) {
		 hibernateTemplate.saveOrUpdate(personDegree);
		 return personDegree;
	}

	@Override
	public void deletePersonDegreeById(Integer personDegreeId) {
		hibernateTemplate.delete(hibernateTemplate.get(PersonDegree.class, personDegreeId));
	}

	@Override
	public List<DegreeType> getDegreeType() {
		return hibernateTemplate.loadAll(DegreeType.class);
	}

	@Override
	public void updateOldPersonUsername(String personId) {
		Session session = null;
		try {
			 session = hibernateTemplate.getSessionFactory().openSession();
			Transaction transaction = session.beginTransaction();
			org.hibernate.query.Query query = session.createQuery("UPDATE Person person SET person.principalName = CONCAT('%', person.principalName), person.status = 'I' WHERE " +
					"person.personId = :personId");
			query.setParameter("personId", personId);
			query.executeUpdate();
			transaction.commit();
		} catch (Exception e) {
			logger.error("Exception in updateOldPersonUsername {}", e.getMessage());
		} finally {
			if(session != null) {
				session.close();
			}
		}
	}

	@Override
	public List<Person> fetchPersonsByLikeUserName(String principalName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		org.hibernate.query.Query query = session.createQuery("SELECT person FROM Person person WHERE REPLACE( person.principalName, '%','') = :principalName");
		query.setParameter("principalName", principalName);
		return query.getResultList();
	}

	@Override
	public List<String> getAdministratorsByModuleCode(Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT DISTINCT pr.personId FROM PersonRoles pr WHERE pr.roleId IN (SELECT ag.roleId FROM AdminGroup ag WHERE ag.moduleCode = :moduleCode)";
		return session.createQuery(hqlQuery, String.class).setParameter("moduleCode", moduleCode).getResultList();
	}

}
