package com.polus.fibicomp.globalentity.dao;

import static java.util.Map.entry;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;

import javax.persistence.EntityNotFoundException;
import javax.persistence.Query;

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
import com.polus.fibicomp.globalentity.dto.EntityRequestField;
import com.polus.fibicomp.globalentity.dto.ValidateDuplicateRequestDTO;
import com.polus.fibicomp.globalentity.pojo.Entity;

import lombok.extern.slf4j.Slf4j;
import oracle.jdbc.OracleTypes;

@Repository
@Transactional
@Slf4j
public class EntityDetailsDAOImpl implements EntityDetailsDAO {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	@Value("${oracledb}")
	private String oracledb;

	protected static Logger logger = LogManager.getLogger(EntityDetailsDAOImpl.class.getName());

	private static final Map<EntityRequestField, String> FIELD_MAPPINGS = Map.ofEntries(
			entry(EntityRequestField.entityName, "entityName"), entry(EntityRequestField.entityNumber, "entityNumber"),
			entry(EntityRequestField.entityOwnershipTypeCode, "entityOwnershipTypeCode"),
			entry(EntityRequestField.primaryAddressLine1, "primaryAddressLine1"),
			entry(EntityRequestField.primaryAddressLine2, "primaryAddressLine2"),
			entry(EntityRequestField.city, "city"), entry(EntityRequestField.state, "state"),
			entry(EntityRequestField.postCode, "postCode"), entry(EntityRequestField.countryCode, "countryCode"),
			entry(EntityRequestField.certifiedEmail, "certifiedEmail"),
			entry(EntityRequestField.websiteAddress, "websiteAddress"),
			entry(EntityRequestField.dunsNumber, "dunsNumber"), entry(EntityRequestField.ueiNumber, "ueiNumber"),
			entry(EntityRequestField.cageNumber, "cageNumber"),
			entry(EntityRequestField.humanSubAssurance, "humanSubAssurance"),
			entry(EntityRequestField.anumalWelfareAssurance, "anumalWelfareAssurance"),
			entry(EntityRequestField.animalAccreditation, "animalAccreditation"),
			entry(EntityRequestField.phoneNumber, "phoneNumber"), entry(EntityRequestField.approvedBy, "approvedBy"),
			entry(EntityRequestField.approvedTimestamp, "approvedTimestamp"),
			entry(EntityRequestField.entityStatusTypeCode, "entityStatusTypeCode"),
			entry(EntityRequestField.isDunsMatched, "isDunsMatched"),
			entry(EntityRequestField.documentStatusTypeCode, "documentStatusTypeCode"),
			entry(EntityRequestField.originalEntityId, "originalEntityId"));

	@Override
	public Integer createEntity(Entity entity) {
		hibernateTemplate.save(entity);
		updateEntity(EntityRequestDTO.builder().entityNumber(entity.getEntityId()).entityId(entity.getEntityId()).build());
		return entity.getEntityId();
	}

	@Override
	public void updateEntity(EntityRequestDTO dto) {
		Map<EntityRequestField, Object> entityRequestFields = dto.getEntityRequestFields();

		if (entityRequestFields == null || entityRequestFields.isEmpty()) {
			String errorMsg = "No fields to update for entity with ID " + dto.getEntityId();
			log.warn(errorMsg);
			return;
		}

		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();

		StringBuilder hqlQuery = new StringBuilder("UPDATE Entity e SET e.updatedBy = :updatedBy, e.updateTimestamp = :updateTimestamp");
		StringJoiner updates = new StringJoiner(", ");

		entityRequestFields.forEach((field, value) -> {
			String fieldName = FIELD_MAPPINGS.get(field);
			if (fieldName != null) {
				updates.add("e." + fieldName + " = :" + fieldName);
			} else {
				String errorMsg = "Unknown field: " + field;
				log.error(errorMsg);
				throw new IllegalArgumentException("Unknown field: " + field);
			}
		});

		hqlQuery.append(", ").append(updates.toString());
		hqlQuery.append(" WHERE e.entityId = :entityId");

		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("entityId", dto.getEntityId());
		query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
		query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());

		entityRequestFields.forEach((field, value) -> {
			query.setParameter(FIELD_MAPPINGS.get(field), value);
		});

		try {
			int updatedRows = query.executeUpdate();
			if (updatedRows == 0) {
				String errorMsg = "Entity with ID " + dto.getEntityId() + " not found.";
				log.warn(errorMsg);
				throw new EntityNotFoundException(errorMsg);
			}
		} catch (Exception e) {
			log.error("Failed to update entity with ID " + dto.getEntityId(), e);
			throw new RuntimeException("Error updating entity with ID " + dto.getEntityId(), e);
		}
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

	@SuppressWarnings("unchecked")
	@Override
	public List<Entity> validateDuplicateByParams(ValidateDuplicateRequestDTO dto) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hql = new StringBuilder("SELECT e FROM Entity e WHERE e.countryCode = :countryCode");
		hql.append(" AND (LOWER(TRIM(e.entityName)) LIKE :entityName");
		hql.append(" OR (e.primaryAddressLine1 IS NOT NULL AND TRIM(e.primaryAddressLine1) != '' ");
		hql.append(" AND LOWER(TRIM(e.primaryAddressLine1)) LIKE :primaryAddressLine1)");
		hql.append(" OR (e.primaryAddressLine2 IS NOT NULL AND TRIM(e.primaryAddressLine2) != '' ");
		hql.append(" AND LOWER(TRIM(e.primaryAddressLine2)) LIKE :primaryAddressLine1)");
		if (dto.getPrimaryAddressLine2() != null && !dto.getPrimaryAddressLine2().trim().isEmpty()) {
		    hql.append(" OR (e.primaryAddressLine2 IS NOT NULL AND TRIM(e.primaryAddressLine2) != '' ");
		    hql.append(" AND LOWER(TRIM(e.primaryAddressLine2)) LIKE :primaryAddressLine2)");
		    hql.append(" OR (e.primaryAddressLine1 IS NOT NULL AND TRIM(e.primaryAddressLine1) != '' ");
		    hql.append(" AND LOWER(TRIM(e.primaryAddressLine1)) LIKE :primaryAddressLine2)");
		}
		hql.append(" )");
		Query query = session.createQuery(hql.toString());
		query.setParameter("entityName", "%" + dto.getEntityName().trim().toLowerCase() + "%");
		query.setParameter("primaryAddressLine1", "%" + dto.getPrimaryAddressLine1().trim().toLowerCase() + "%");
		if (dto.getPrimaryAddressLine2() != null && !dto.getPrimaryAddressLine2().trim().isEmpty()) {
		    query.setParameter("primaryAddressLine2", "%" + dto.getPrimaryAddressLine2().trim().toLowerCase() + "%");
		}
		query.setParameter("countryCode", dto.getCountryCode());
		return query.getResultList();
	}

	@SuppressWarnings("unused")
	@Override
	public void updateDocWithOriginalEntity(Integer duplicateEntityId, Integer originalEntityId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call UPD_DOCS_WITH_ORG_ENTITY(?,?)}");
				statement.setInt(1, duplicateEntityId);
				statement.setInt(2, originalEntityId);
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call UPD_DOC_WITH_ORG_ENTITY(?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(1, duplicateEntityId);
				statement.setInt(2, originalEntityId);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
		} catch (Exception e) {
			logger.error("Exception on updateDocWithOriginalEntity {}", e.getMessage());
			throw new ApplicationException("Exception on updateDocWithOriginalEntity", e, Constants.DB_PROC_ERROR);
		}
	}

}
