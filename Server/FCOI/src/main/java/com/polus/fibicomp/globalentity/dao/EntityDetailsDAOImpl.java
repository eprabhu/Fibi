package com.polus.fibicomp.globalentity.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
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
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.applicationexception.dto.ApplicationException;
import com.polus.core.common.dao.CommonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.globalentity.dto.EntityRequestDTO;
import com.polus.fibicomp.globalentity.dto.ValidateDuplicateRequestDTO;
import com.polus.fibicomp.globalentity.pojo.Entity;

import oracle.jdbc.OracleTypes;

@Repository
@Transactional
public class EntityDetailsDAOImpl implements EntityDetailsDAO {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	@Value("${oracledb}")
	private String oracledb;

	protected static Logger logger = LogManager.getLogger(EntityDetailsDAOImpl.class.getName());

	@Override
	public Integer createEntity(Entity entity) {
		hibernateTemplate.save(entity);
		updateEntity(
				EntityRequestDTO.builder().entityNumber(entity.getEntityId()).entityId(entity.getEntityId()).build());
		return entity.getEntityId();
	}

	@Override
	public void updateEntity(EntityRequestDTO dto) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("UPDATE Entity e SET e.updatedBy = :updatedBy, e.updateTimestamp = :updateTimestamp ");
		if (dto.getEntityName() != null) {
			hqlQuery.append(", e.entityName = :entityName");
		}
		if (dto.getEntityNumber() != null) {
			hqlQuery.append(", e.entityNumber = :entityNumber");
		}
		if (dto.getEntityOwnershipTypeCode() != null) {
			hqlQuery.append(", e.entityOwnershipTypeCode = :entityOwnershipTypeCode");
		}
		if (dto.getPrimaryAddressLine1() != null) {
			hqlQuery.append(", e.primaryAddressLine1 = :primaryAddressLine1");
		}
		if (dto.getPrimaryAddressLine2() != null) {
			hqlQuery.append(", e.primaryAddressLine2 = :primaryAddressLine2");
		}
		if (dto.getCity() != null) {
			hqlQuery.append(", e.city = :city");
		}
		if (dto.getState() != null) {
			hqlQuery.append(", e.state = :state");
		}
		if (dto.getPostCode() != null) {
			hqlQuery.append(", e.postCode = :postCode");
		}
		if (dto.getCountryCode() != null) {
			hqlQuery.append(", e.countryCode = :countryCode");
		}
		if (dto.getCertifiedEmail() != null) {
			hqlQuery.append(", e.certifiedEmail = :certifiedEmail");
		}
		if (dto.getWebsiteAddress() != null) {
			hqlQuery.append(", e.websiteAddress = :websiteAddress");
		}
		if (dto.getDunsNumber() != null) {
			hqlQuery.append(", e.dunsNumber = :dunsNumber");
		}
		if (dto.getUeiNumber() != null) {
			hqlQuery.append(", e.ueiNumber = :ueiNumber");
		}
		if (dto.getCageNumber() != null) {
			hqlQuery.append(", e.cageNumber = :cageNumber");
		}
		if (dto.getHumanSubAssurance() != null) {
			hqlQuery.append(", e.humanSubAssurance = :humanSubAssurance");
		}
		if (dto.getAnumalWelfareAssurance() != null) {
			hqlQuery.append(", e.anumalWelfareAssurance = :anumalWelfareAssurance");
		}
		if (dto.getAnimalAccreditation() != null) {
			hqlQuery.append(", e.animalAccreditation = :animalAccreditation");
		}
		if (dto.getPhoneNumber() != null) {
			hqlQuery.append(", e.phoneNumber = :phoneNumber");
		}
		if (dto.getApprovedBy() != null) {
			hqlQuery.append(", e.approvedBy = :approvedBy");
		}
		if (dto.getApprovedTimestamp() != null) {
			hqlQuery.append(", e.approvedTimestamp = :approvedTimestamp");
		}
		if (dto.getEntityStatusTypeCode() != null) {
			hqlQuery.append(", e.entityStatusTypeCode = :entityStatusTypeCode");
		}
		if (dto.getIsDunsMatched() != null) {
			hqlQuery.append(", e.isDunsMatched = :isDunsMatched");
		}
		if (dto.getDocumentStatusTypeCode() != null) {
			hqlQuery.append(", e.documentStatusTypeCode = :documentStatusTypeCode");
		}
		if (dto.getOriginalEntityId() != null) {
			hqlQuery.append(", e.originalEntityId = :originalEntityId");
		}
		hqlQuery.append(" WHERE e.entityId = :entityId");
		Query query = session.createQuery(hqlQuery.toString());
		if (dto.getEntityName() != null) {
			query.setParameter("entityName", dto.getEntityName());
		}
		if (dto.getEntityOwnershipTypeCode() != null) {
			query.setParameter("entityOwnershipTypeCode", dto.getEntityOwnershipTypeCode());
		}
		if (dto.getPrimaryAddressLine1() != null) {
			query.setParameter("primaryAddressLine1", dto.getPrimaryAddressLine1());
		}
		if (dto.getPrimaryAddressLine2() != null) {
			query.setParameter("primaryAddressLine2", dto.getPrimaryAddressLine2());
		}
		if (dto.getCity() != null) {
			query.setParameter("city", dto.getCity());
		}
		if (dto.getState() != null) {
			query.setParameter("state", dto.getState());
		}
		if (dto.getPostCode() != null) {
			query.setParameter("postCode", dto.getPostCode());
		}
		if (dto.getCountryCode() != null) {
			query.setParameter("countryCode", dto.getCountryCode());
		}
		if (dto.getCertifiedEmail() != null) {
			query.setParameter("certifiedEmail", dto.getCertifiedEmail());
		}
		if (dto.getWebsiteAddress() != null) {
			query.setParameter("websiteAddress", dto.getWebsiteAddress());
		}
		if (dto.getDunsNumber() != null) {
			query.setParameter("dunsNumber", dto.getDunsNumber());
		}
		if (dto.getUeiNumber() != null) {
			query.setParameter("ueiNumber", dto.getUeiNumber());
		}
		if (dto.getCageNumber() != null) {
			query.setParameter("cageNumber", dto.getCageNumber());
		}
		if (dto.getHumanSubAssurance() != null) {
			query.setParameter("humanSubAssurance", dto.getHumanSubAssurance());
		}
		if (dto.getAnumalWelfareAssurance() != null) {
			query.setParameter("anumalWelfareAssurance", dto.getAnumalWelfareAssurance());
		}
		if (dto.getAnimalAccreditation() != null) {
			query.setParameter("animalAccreditation", dto.getAnimalAccreditation());
		}
		if (dto.getPhoneNumber() != null) {
			query.setParameter("phoneNumber", dto.getPhoneNumber());
		}
		if (dto.getEntityNumber() != null) {
			query.setParameter("entityNumber", dto.getEntityNumber());
		}
		if (dto.getApprovedBy() != null) {
			query.setParameter("approvedBy", dto.getApprovedBy());
		}
		if (dto.getApprovedTimestamp() != null) {
			query.setParameter("approvedTimestamp", dto.getApprovedTimestamp());
		}
		if (dto.getEntityStatusTypeCode() != null) {
			query.setParameter("entityStatusTypeCode", dto.getEntityStatusTypeCode());
		}
		if (dto.getIsDunsMatched() != null) {
			query.setParameter("isDunsMatched", dto.getIsDunsMatched());
		}
		if (dto.getDocumentStatusTypeCode() != null) {
			query.setParameter("documentStatusTypeCode", dto.getDocumentStatusTypeCode());
		}
		if (dto.getOriginalEntityId() != null) {
			query.setParameter("originalEntityId", dto.getOriginalEntityId());
		}
		query.setParameter("entityId", dto.getEntityId());
		query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
		query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());
		query.executeUpdate();
	}

	@Override
	public Entity fetchEntityDetails(Integer entityId) {
		return hibernateTemplate.get(Entity.class, entityId);
	}

	@Override
	public Map<String, Object> getEntityTabStatus(Integer entityId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		Map<String, Object> entityTabStatus = new HashMap<>();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_ENTITY_TAB_STATUS(?)}");
				statement.setInt(1, entityId);
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call GET_ENTITY_TAB_STATUS(?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(1, entityId);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
			while (rset != null && rset.next()) {
				entityTabStatus.put("entity_sub_org_info", rset.getBoolean("entity_sub_org_info"));
				entityTabStatus.put("entity_sponsor_info", rset.getBoolean("entity_sponsor_info"));
				entityTabStatus.put("entity_overview", rset.getBoolean("entity_overview"));
				entityTabStatus.put("sponsor_feed_status", rset.getString("sponsor_feed_status") != null ? rset.getString("sponsor_feed_status") : "");
				entityTabStatus.put("organization_feed_status", rset.getString("organization_feed_status") != null ? rset.getString("organization_feed_status") : "");
				entityTabStatus.put("sponsor_feed_status_code", rset.getString("sponsor_feed_status_code") != null ? rset.getString("sponsor_feed_status_code") : "");
				entityTabStatus.put("organization_feed_status_code", rset.getString("organization_feed_status_code") != null ? rset.getString("organization_feed_status_code") : "");
				entityTabStatus.put("organization_id", rset.getString("organization_id") != null ? rset.getString("organization_id") : "");
				entityTabStatus.put("sponsor_code", rset.getString("sponsor_code") != null ? rset.getString("sponsor_code") : "");
			}
		} catch (Exception e) {
			logger.error("Exception on getEntityTabStatus {}", e.getMessage());
			throw new ApplicationException("Unable to fetch entity tab status", e, Constants.DB_PROC_ERROR);
		}
		return entityTabStatus;
	}

	@Override
	public List<Entity> validateDuplicateByParams(ValidateDuplicateRequestDTO dto) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaQuery<Entity> cq = cb.createQuery(Entity.class);
		Root<Entity> entity = cq.from(Entity.class);
		Predicate entityNamePredicate = cb.like(cb.lower(cb.trim(entity.get("entityName"))), "%" + dto.getEntityName().trim().toLowerCase() + "%");
		Predicate addressLine1Predicate1 = cb.like(cb.lower(cb.trim(entity.get("primaryAddressLine1"))), "%" + dto.getPrimaryAddressLine1().trim().toLowerCase() + "%");
		Predicate addressLine1Predicate2 = cb.like(cb.lower(cb.trim(entity.get("primaryAddressLine2"))), "%" + dto.getPrimaryAddressLine1().trim().toLowerCase() + "%");
		Predicate addressLine2Predicate1 = cb.like(cb.lower(cb.trim(entity.get("primaryAddressLine2"))), "%" + dto.getPrimaryAddressLine2().trim().toLowerCase() + "%");
		Predicate addressLine2Predicate2 = cb.like(cb.lower(cb.trim(entity.get("primaryAddressLine1"))), "%" + dto.getPrimaryAddressLine2().trim().toLowerCase() + "%");
		Predicate countryPredicate = cb.equal(entity.get("countryCode"), dto.getCountryCode());
		Predicate finalPredicate = cb.and(
		    cb.or(
		        entityNamePredicate, 
		        addressLine1Predicate1, 
		        addressLine1Predicate2, 
		        addressLine2Predicate1, 
		        addressLine2Predicate2
		    ), 
		    countryPredicate
		);
		cq.where(finalPredicate);
		return session.createQuery(cq).getResultList();
	}

}
