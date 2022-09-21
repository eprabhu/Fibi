package com.polus.fibicomp.externaluser.dao;

import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.common.service.DateTimeService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.externaluser.pojo.ExternalUser;
import com.polus.fibicomp.externaluser.pojo.ExternalUserFeed;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.person.pojo.PersonRoleRT;
import com.polus.fibicomp.person.pojo.PersonRoleRTAttributes;
import com.polus.fibicomp.roles.pojo.PersonRoles;
import com.polus.fibicomp.roles.pojo.Rights;

@Transactional
@Service (value = "externalUserDao")
public class ExternalUserDaoImpl implements ExternalUserDao {
	
	protected static Logger logger = LogManager.getLogger(ExternalUserDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;
	
	@Autowired
	private DateTimeService dateTimeService;

	@Override
	public List<ExternalUser> fetchAllExternalUserDetails(List<String> unit) {
		List<ExternalUser> details = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ExternalUser> query = builder.createQuery(ExternalUser.class);
		Root<ExternalUser> externalUser = query.from(ExternalUser.class);
		Predicate predicateFlg = externalUser.get("fundingOffice").in(unit);
		query.where(builder.and(predicateFlg));
		details = session.createQuery(query).getResultList();
		return details;
	}

	@Override
	public void updateApproveReject(ExternalUser externalUser) {
		try {
			hibernateTemplate.saveOrUpdate(externalUser);
		} catch (Exception e) {
			logger.error("Exception in saveApproveReject : {}", e.getMessage());
		}		
	}

	@Override
	public ExternalUser getExternalUserByUserName(String userName) {
		ExternalUser details = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "select t1 from ExternalUser t1 where t1.userName=:userName and t1.verifiedFlag<> :verifiedFlag";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("userName", userName);
		query.setParameter("verifiedFlag", Constants.REJECTED_USER);
		details = (ExternalUser) query.getSingleResult();
		return details;
	}

	@Override
	public ExternalUser getExternalUserByPersonId(Integer personId) {
		return hibernateTemplate.get(ExternalUser.class, personId);
	}

	@Override
	public ExternalUserFeed saveExternalUserFeed(ExternalUserFeed userFeed) {
		try {
			hibernateTemplate.saveOrUpdate(userFeed);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return userFeed;
		
	}

	@Override
	public List<ExternalUserFeed> getExternalUserFeedDetailsForAdd() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ExternalUserFeed> query = builder.createQuery(ExternalUserFeed.class);
		Root<ExternalUserFeed> externalUserFeed = query.from(ExternalUserFeed.class);
		Predicate predicateFlg = builder.equal(externalUserFeed.get("isSent"), Constants.NO);
		Predicate predicateAdd = builder.equal(externalUserFeed.get("action"), Constants.ADD);
		query.where(builder.and(predicateFlg, predicateAdd));
		List<ExternalUserFeed> feedList = session.createQuery(query).getResultList();
		if(feedList != null && !feedList.isEmpty()) {
			return feedList;
		}
		return null;
	}

	@Override
	public void saveOrUpdate(ExternalUserFeed userDetails) {
		try {
			hibernateTemplate.saveOrUpdate(userDetails);
		} catch (Exception e) {
			e.printStackTrace();
		}		
	}

	@Override
	public List<ExternalUserFeed> getExternalUserFeedDetailsForDelete() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ExternalUserFeed> query = builder.createQuery(ExternalUserFeed.class);
		Root<ExternalUserFeed> externalUserFeed = query.from(ExternalUserFeed.class);
		Predicate predicateFlg = builder.equal(externalUserFeed.get("isSent"), Constants.NO);
		Predicate predicateDelete = builder.equal(externalUserFeed.get("action"), Constants.DELETE);
		query.where(builder.and(predicateFlg, predicateDelete));
		List<ExternalUserFeed> feedList = session.createQuery(query).getResultList();
		if(feedList != null && !feedList.isEmpty()) {
			return feedList;
		}
		return null;
	}

	@Override
	public List<ExternalUserFeed> getExternalUserFeeds() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ExternalUserFeed> query = builder.createQuery(ExternalUserFeed.class);
		Root<ExternalUserFeed> externalUserFeed = query.from(ExternalUserFeed.class);
		Predicate add = builder.equal(externalUserFeed.get("action"), Constants.ADD);
		Predicate predicateFlg = builder.equal(externalUserFeed.get("isSent"), Constants.EXTERNAL_USER_FEED_STATUS);
		query.where(builder.and(add,predicateFlg));
		List<ExternalUserFeed> feedList = session.createQuery(query).getResultList();
		if (feedList != null && !feedList.isEmpty()) {
			return feedList;
		}
		return null;
	}

	@SuppressWarnings({"unchecked" })
	public List<Rights> getPersonRightFromRole(Integer roleId) {
		List<Rights> rights = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT t1 FROM Rights t1 WHERE t1.rightId IN (SELECT t2.rightId FROM RoleRights t2 WHERE t2.roleId=:roleId)";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("roleId", roleId);
		rights = query.getResultList();
		return rights;
	}

	@Override
	public void assignPersonRole(Person person, Integer roleId) {
		try {
			if (checkRoleForPerson(person.getHomeUnit(), person.getPersonId(), roleId)) {
				PersonRoles personRole = new PersonRoles();
				personRole.setPersonId(person.getPersonId());
				personRole.setRoleId(roleId);
				personRole.setUnitNumber(person.getHomeUnit());
				personRole.setDescentFlag(Constants.NO);
				personRole.setUpdateTimeStamp(dateTimeService.getCurrentTimestamp());
				personRole.setUpdateUser("quickstart");
				hibernateTemplate.saveOrUpdate(personRole);
			}
		} catch (Exception e) {
			logger.info("Exception in assignPersonRole : {}", e);
			e.printStackTrace();
		}
	}

	private boolean checkRoleForPerson(String homeUnit, String personId, Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "select count(*) from PersonRoles where personId =: personId and unitNumber =: unitNumber and roleId =: roleId";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("personId", personId);
		query.setParameter("unitNumber", homeUnit);
		query.setParameter("roleId", roleId);
		Integer count = Integer.parseInt(query.getSingleResult().toString());
		return count == 0 ? true : false;
	}

	@Override
	public void assignPersonRoleRT(Person person, Integer roleId) {
		try {
			List<Rights> personRights = getPersonRightFromRole(roleId);
			if (personRights != null) {
				for (Rights right : personRights) {
					logger.info("Person Id: {}", person.getPersonId());
					logger.info("Right Name: {}", right.getRightName());
					logger.info("Home Unit: {}", person.getHomeUnit());
					if (checkRightForPersonRoleRT(person.getHomeUnit(), person.getPersonId(), roleId, right.getRightName())) {
						PersonRoleRT personRoleRT = new PersonRoleRT();
						PersonRoleRTAttributes personRoleRTAttributes = new PersonRoleRTAttributes();
						personRoleRT.setRoleId(roleId);
						personRoleRTAttributes.setPersonId(person.getPersonId());
						personRoleRTAttributes.setUnitNumber(person.getHomeUnit());
						personRoleRTAttributes.setRightName(right.getRightName());
						personRoleRT.setPersonRoleRTAttributes(personRoleRTAttributes);
						hibernateTemplate.saveOrUpdate(personRoleRT);
					}
				}
			}
		} catch (Exception e) {
			logger.info("Exception in assignPersonRoleRT : {}", e);
			e.printStackTrace();
		}
	}

	private boolean checkRightForPersonRoleRT(String homeUnit, String personId, Integer roleId, String rightName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hqlQuery = new StringBuilder("select count(*) from PersonRoleRT where personRoleRTAttributes.personId =: personId and ").
				append("personRoleRTAttributes.unitNumber =: unitNumber and roleId =: roleId and personRoleRTAttributes.rightName =: rightName");		
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("personId", personId);
		query.setParameter("unitNumber", homeUnit);
		query.setParameter("roleId", roleId);
		query.setParameter("rightName", rightName);
		Integer count = Integer.parseInt(query.getSingleResult().toString());
		return count == 0 ? true : false;
	}

	@Override
	public String getHomeUnitFromOrganizationId(String organizationId) {
		String unit = null;
		try {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT t1.unitNumber FROM Unit t1 WHERE t1.organizationId =:organizationId";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("organizationId", organizationId);
		unit = (String) query.getSingleResult();
		return unit;
		} catch (Exception e) {
			logger.info("Exception in getHomeUnitFromOrganizationId: {}", e);
		}
		return unit;
	}

	@Override
	public List<ExternalUser> getPendingExternalUserList() {
		List<ExternalUser> details = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ExternalUser> query = builder.createQuery(ExternalUser.class);
		Root<ExternalUser> externalUser = query.from(ExternalUser.class);
		Predicate predicateFlg = builder.equal(externalUser.get("verifiedFlag"), Constants.PENDING_USER);
		Predicate isSentFlag = builder.equal(externalUser.get("isMailSent"), Constants.NO);
		query.where(builder.and(predicateFlg, isSentFlag));
		details = session.createQuery(query).getResultList();
		return details;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> getUnitsListByPersonIdAndRights(String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT T1.personRoleRTAttributes.unitNumber from PersonRoleRT T1 where T1.personRoleRTAttributes.personId =:personId and T1.personRoleRTAttributes.rightName =:rightName";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("personId", personId);
		query.setParameter("rightName", "MAINTAIN_EXTERNAL_USER");
		List<String> units = query.getResultList();
		if (units != null && !units.isEmpty()) {
			return units;
		}
		return null;
	}

}
