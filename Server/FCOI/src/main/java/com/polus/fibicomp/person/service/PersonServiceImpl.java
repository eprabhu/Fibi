package com.polus.fibicomp.person.service;

import java.security.GeneralSecurityException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.person.vo.PersonSearchResult;
import com.polus.fibicomp.person.vo.PersonVO;
import com.polus.fibicomp.roles.pojo.PersonRoles;
import com.polus.fibicomp.security.AuthenticatedUser;

@Transactional
@Service(value = "personService")
public class PersonServiceImpl implements PersonService {
	
	protected static Logger logger = LogManager.getLogger(PersonServiceImpl.class.getName());
	
	@Autowired
	private PersonDao personDao;
	
	@Autowired
	private CommonDao commonDao;
	
	@Autowired
	private CommonService commonService;
	
	@Autowired
    private HibernateTemplate hibernateTemplate;
       
    @Value("${oracledb}")
    private String oracledb;

	private static final boolean TRUE = true;
	private static final boolean FALSE = false;

	@Override
	public ResponseEntity<String> getPersonDetailById(PersonVO vo) {
		String personId = vo.getPersonId();
		HttpStatus httpStatus = HttpStatus.OK;
		if ((vo.getPersonId() != null && vo.getPersonId().equals(AuthenticatedUser.getLoginPersonId()))
				|| personDao.isPersonHasPermissionInAnyDepartment(AuthenticatedUser.getLoginPersonId(),
						Constants.MAINTAIN_PERSON_RIGHT_NAME)) {
			Person person = personDao.getPersonDetailById(personId);
			vo.setPerson(person);
			return new ResponseEntity<>(commonDao.convertObjectToJSON(vo), httpStatus);
		}
		httpStatus = HttpStatus.FORBIDDEN;
		return new ResponseEntity<>("Not Authorized to view this details", httpStatus);
	}

	@Override
	public List<PersonSearchResult> findPerson(String searchString) {
		return personDao.findPerson(searchString);
	}

	@Override
	public String saveOrUpdatePerson(PersonVO vo) {
		Person person = vo.getPerson();
		String acType = vo.getAcType();
		try {
			if (person.getIsPasswordChange().equals(TRUE)) {
				person.setPassword(commonService.hash(person.getPassword()));
			} else {
				person.setPassword(personDao.getPersonPassword(person.getPersonId()));
			}
			if (acType != null && acType.equals("I")) {
				if (personDao.checkUniquePrincipalName(person.getPrincipalName()) == FALSE) {
					 if (oracledb.equalsIgnoreCase("Y")) {
                         String nextSequenceId = getNextSeq("SEQ_PERSON_ID");
                         logger.info("getNextSeq : " +nextSequenceId);
                         person.setPersonId(nextSequenceId);
                 } else {
//                         String nextSequenceId = getNextSeqMySql("SELECT IFNULL(MAX(cast(PERSON_ID as decimal(40,0))),1000)+1  FROM PERSON");
                         String nextSequenceId = getNextSeqMySql("SELECT IFNULL(MAX(CONVERT(PERSON_ID, SIGNED INTEGER)),100)+1 FROM PERSON");
                         logger.info("getNextSeq : " + nextSequenceId);
                         person.setPersonId(nextSequenceId);
                 }

					person = personDao.saveOrUpdatePerson(person);
					vo.setMessage("Person saved Successfully");
					vo.setPerson(person);
					vo.setAcType("U");
				} else {
					vo.setMessage("Username already exists");
					vo.setPerson(null);
				}
			} else if (acType != null && acType.equals("U")) {
				if (person.getIsUsernameChange().equals(TRUE)) {
					if (personDao.checkUniquePrincipalName(person.getPrincipalName()) == FALSE) {
						person = personDao.saveOrUpdatePerson(person);
						vo.setMessage("Person updated Successfully");
						vo.setPerson(person);
					} else {
						vo.setMessage("Username already exists");
						vo.setPerson(null);
					}
				} else {
					person = personDao.saveOrUpdatePerson(person);
					vo.setMessage("Person updated Successfully");
					vo.setPerson(person);
				}
			} else if (acType != null && acType.equals("D")) {
				personDao.deletePerson(person);
				vo.setMessage("Person deleted successfully");
			}
		} catch (GeneralSecurityException e) {
			logger.error("error in saveOrUpdatePerson: {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getAllPersons(PersonVO vo) {
		return commonDao.convertObjectToJSON(personDao.getAllPersons(vo));
	}

	@Override
	public String getAllTrainings(PersonVO vo) {
		return commonDao.convertObjectToJSON(personDao.getAllTrainings(vo));
	}

	
	private String getNextSeq(String sequence) {
        logger.info("----------- this method generate id not auto incriment Service ------------");
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        Statement statement;
        int nextSeq = 0;
        try {
                statement = connection.createStatement();
                ResultSet rs = statement.executeQuery("select +" + sequence + ".nextval from dual");
                if (rs.next()) {
                        nextSeq = rs.getInt(1);
                }
        } catch (SQLException e) {
                e.printStackTrace();
        }
        return Integer.toString(nextSeq);
	}
	
	private String getNextSeqMySql(String sql) {
	        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
	        SessionImpl sessionImpl = (SessionImpl) session;
	        Connection connection = sessionImpl.connection();
	        Statement statement;
	        String nextSeq = null;
	        try {
	                statement = connection.createStatement();
	                ResultSet rs = statement.executeQuery(sql);
	
	                if (rs.next()) {
	                        nextSeq = rs.getString(1);
	                }
	        } catch (SQLException e) {
	                e.printStackTrace();
	        }
	        return nextSeq;
	}
	
	@Override
	public String savePersonFromFeed(Person person) {
		String message = personDao.savePersonFromFeed(person);
		return message;
	}

	@Override
	public Set<Person> getPersonBasedOnRoleAndRight() {
		List <PersonRoles> personRoles = personDao.getPersonRolesByRoleAndRightId(Constants.AGREEMENT_ADMINISTRATOR, Constants.VIEW_ADMIN_GROUP_AGREEMENT);
		Set<Person> persons = new HashSet<>();
		personRoles.forEach(personRole -> {
			if (personRole.getPerson() != null) {
				persons.add(personRole.getPerson());
			}
		});
		return persons;
	}

	@Override
	public String savePersonDegree(PersonVO vo) {
		try {
			vo.getPersonDegree().setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			vo.getPersonDegree().setUpdateUser(AuthenticatedUser.getLoginUserName());
			personDao.savePersonDegree(vo.getPersonDegree());
		}catch(Exception e) {
			throw new ApplicationException("save Person Degree", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(personDao.getAllPersonDegree(vo.getPersonDegree().getPersonId()));
	}

	@Override
	public String getAllPersonDegree(PersonVO vo) {
		return commonDao.convertObjectToJSON(personDao.getAllPersonDegree(vo.getPersonId()));
	}

	@Override
	public String deletePersonDegree(Integer personDegreeId) {
		try {
			personDao.deletePersonDegreeById(personDegreeId);
			return commonDao.convertObjectToJSON("deleted successfully");
		}catch(Exception e) {
			throw new ApplicationException("deletePersonDegreeById",e, Constants.DB_PROC_ERROR);
		}
	}

	@Override
	public String getDegreeType() {
		return commonDao.convertObjectToJSON(personDao.getDegreeType());
	}
}
