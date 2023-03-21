package com.polus.fibicomp.orcid.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
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

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.orcid.pojo.AwardPersonOrcidWork;
import com.polus.fibicomp.orcid.pojo.OrcidErrorLog;
import com.polus.fibicomp.orcid.pojo.OrcidWebhookNotificationLog;
import com.polus.fibicomp.orcid.pojo.OrcidWork;
import com.polus.fibicomp.orcid.pojo.OrcidWorkCategory;
import com.polus.fibicomp.orcid.pojo.OrcidWorkContributor;
import com.polus.fibicomp.orcid.pojo.OrcidWorkExternalIdentifier;
import com.polus.fibicomp.orcid.pojo.OrcidWorkType;
import com.polus.fibicomp.orcid.pojo.PersonOrcidWork;
import com.polus.fibicomp.person.pojo.Person;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "orcidDao")
public class OrcidDaoImpl implements OrcidDao {

	protected static Logger logger = LogManager.getLogger(OrcidDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private CommonDao commonDao;

	private static final String UPDATE_USER = "quickstart";

	@SuppressWarnings("unchecked")
	@Override
	public List<String> getAllOrcidIds() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		List<String> orcidIds = new ArrayList<String>();
 		String hqlQuery = "select P.orcidId FROM Person P WHERE P.orcidId is not null";
		Query query = session.createQuery(hqlQuery);
		if (query.getResultList() != null && !query.getResultList().isEmpty()) {
			orcidIds.addAll(query.getResultList());
		}	
        return orcidIds;
	}

	@Override
	public OrcidWork saveOrUpdateOrcidWork(OrcidWork orcidWork) {
		try {
			hibernateTemplate.saveOrUpdate(orcidWork);
		} catch (Exception e) {
			logger.info("exception in saveOrUpdateOrcidWork : {} ", e.getMessage());
			e.printStackTrace();
			saveOrcidErrorLog(500, "exception in saveOrUpdateOrcidWork" + e.getMessage(), "", orcidWork.getPutCode().toString());
		}
		return orcidWork;
	}

	@Override
	public List<PersonOrcidWork> fetchPersonOrcidWorksBypersonId(String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonOrcidWork> query = builder.createQuery(PersonOrcidWork.class);
		Root<PersonOrcidWork> rootPersonOrcidWork= query.from(PersonOrcidWork.class);
		Predicate predicatePersonId= builder.equal(rootPersonOrcidWork.get("personId"), personId);
		query.where(builder.and(predicatePersonId));
		query.orderBy(builder.asc(rootPersonOrcidWork.get("orcidWork").get("orcidWorkType").get("sortOrder")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public OrcidWork getOrcidWorkById(Integer putCode) {
		return hibernateTemplate.get(OrcidWork.class, putCode);
	}

	@Override
	public String getOrcidWorkCategoryCodeByTypeCode(String workTypeCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT O.orcidWorkCategoryCode FROM OrcidWorkType O WHERE O.orcidWorkTypeCode = :orcidWorkTypeCode";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("orcidWorkTypeCode", workTypeCode);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	public AwardPersonOrcidWork saveOrUpdateAwardPersonOrcidWork(AwardPersonOrcidWork orcidWork) {
		try {
			hibernateTemplate.saveOrUpdate(orcidWork);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateAwardPersonOrcidWork : {} ", e.getMessage());
		}
		return orcidWork;
	}

	@Override
	public List<OrcidWorkCategory> getAllOrcidWorkCateogories() {
		return hibernateTemplate.loadAll(OrcidWorkCategory.class);
	}

	@Override
	public List<OrcidWorkType> getAllOrcidWorkTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<OrcidWorkType> query = builder.createQuery(OrcidWorkType.class);
		Root<OrcidWorkType> rootOrcidWorkType= query.from(OrcidWorkType.class);
		query.orderBy(builder.asc(rootOrcidWorkType.get("sortOrder")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteOrcidWorkConributor(Integer putCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<OrcidWorkContributor> delete = builder.createCriteriaDelete(OrcidWorkContributor.class);
		Root<OrcidWorkContributor> root = delete.from(OrcidWorkContributor.class);
		delete.where(builder.equal(root.get("orcidWork").get("putCode"), putCode));
		session.createQuery(delete).executeUpdate();
	}

	@Override
	public void deleteOrcidWorkIdentifier(Integer putCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<OrcidWorkExternalIdentifier> delete = builder.createCriteriaDelete(OrcidWorkExternalIdentifier.class);
		Root<OrcidWorkExternalIdentifier> root = delete.from(OrcidWorkExternalIdentifier.class);
		delete.where(builder.equal(root.get("orcidWork").get("putCode"), putCode));
		session.createQuery(delete).executeUpdate();
	}

	@Override
	public String getCountryNameByCountryTwoCode(String countryTwoCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT C.countryName FROM Country C WHERE C.countryTwoCode = :countryTwoCode";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("countryTwoCode", countryTwoCode);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public PersonOrcidWork saveOrUpdatePersonOrcidWork(PersonOrcidWork personOrcidWork) {
		hibernateTemplate.saveOrUpdate(personOrcidWork);
		return personOrcidWork;
	}

	@Override
	public Boolean isPutCodeAlreadyFound(Integer putCode, String orcidId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonOrcidWork> query = builder.createQuery(PersonOrcidWork.class);
		Root<PersonOrcidWork> rootPersonOrcidWork = query.from(PersonOrcidWork.class);
		Predicate predicateOne = builder.equal(rootPersonOrcidWork.get("putCode"), putCode);
		Predicate predicateTwo = builder.equal(rootPersonOrcidWork.get("orcidId"), orcidId);
		query.where(builder.and(predicateOne, predicateTwo));
		List<PersonOrcidWork> personOrcidWork = session.createQuery(query).getResultList();
		if (personOrcidWork != null && !personOrcidWork.isEmpty()) {
			return true;
		} else {
			return false;
		}
	}

	@Override
	public List<OrcidWebhookNotificationLog> getAllOrcidWebhookNotificationLog() {
		return hibernateTemplate.loadAll(OrcidWebhookNotificationLog.class);
	}

	@Override
	public Person getPersonDetailByOrcidId(String orcidId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "FROM Person P WHERE P.orcidId = :orcidId";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("orcidId", orcidId);
			return (Person) query.getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public void deleteAllOrcidWebhookNotificationLog(List<OrcidWebhookNotificationLog> orcidWebhookNotificationLogs) {
		hibernateTemplate.deleteAll(orcidWebhookNotificationLogs);	
	}

	@Override
	public void deleteOrcidWebhookNotificationLog(OrcidWebhookNotificationLog orcidWebhookNotificationLog) {
		hibernateTemplate.delete(orcidWebhookNotificationLog);	
	}

	@Override
	public void saveOrUpdateOrcidWebhookNotificationLog(OrcidWebhookNotificationLog orcidWebhookNotificationLog) {
		hibernateTemplate.saveOrUpdate(orcidWebhookNotificationLog);	
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardPersonOrcidWork> getAwardPersonOrcidWorksByPersonOrcidWorkId(Integer personOrcidWorkId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "FROM AwardPersonOrcidWork A WHERE A.personOrcidWorkId = :personOrcidWorkId";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("personOrcidWorkId", personOrcidWorkId);
			return (List<AwardPersonOrcidWork>) query.getResultList();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public Person getPersonIdByOrcidId(String orcidId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "from Person P WHERE P.orcidId = :orcidId";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("orcidId", orcidId);
			return (Person) query.getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public AwardPersonOrcidWork getAwardPersonOrcidWorkById(Integer awardPersonOrcidWorkId) {
		return hibernateTemplate.get(AwardPersonOrcidWork.class, awardPersonOrcidWorkId);
	}

	@Override
	public void deleteAwardPersonOrcidWork(AwardPersonOrcidWork orcidWork) {
		hibernateTemplate.delete(orcidWork);
	}

	@Override
	public List<AwardPersonOrcidWork> getAwardPersonOrcidWorksByAwardNumber(String awardNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "FROM AwardPersonOrcidWork A WHERE A.awardNumber = :awardNumber";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("awardNumber", awardNumber);
			return (List<AwardPersonOrcidWork>) query.getResultList();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public void saveOrUpdateOrcidErrorLog(OrcidErrorLog orcidErrorLog) {
		try {
			hibernateTemplate.saveOrUpdate(orcidErrorLog);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateOrcidErrorLog : {} ", e.getMessage());
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Integer> getAllPutCodesBasedOnOrcidId(String orcidId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT P.putCode FROM PersonOrcidWork P where P.orcidId = :orcidId";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("orcidId", orcidId);
			return (List<Integer>) query.getResultList();
		} catch (Exception e) {
			return null;
		}
	}

	private void saveOrcidErrorLog(Integer statusCode, String message, String orcidId, String putCode) {
		OrcidErrorLog orcidErrorLog = new OrcidErrorLog();
		orcidErrorLog.setErrorType(statusCode.toString());
		orcidErrorLog.setErrorMessage(message);
		orcidErrorLog.setOrcidId(orcidId);
		if (putCode != null) {
			orcidErrorLog.setPutCode(Integer.parseInt(putCode));
		}
		orcidErrorLog.setUpdateUser(UPDATE_USER);
		orcidErrorLog.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		saveOrUpdateOrcidErrorLog(orcidErrorLog);
	}

}
