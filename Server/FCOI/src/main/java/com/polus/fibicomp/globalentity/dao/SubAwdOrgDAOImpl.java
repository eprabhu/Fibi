package com.polus.fibicomp.globalentity.dao;

import static java.util.Map.entry;

import java.util.Map;
import java.util.StringJoiner;

import javax.persistence.EntityNotFoundException;
import javax.persistence.Query;

import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.globalentity.dto.SubAwardOrgField;
import com.polus.fibicomp.globalentity.dto.SubAwdOrgRequestDTO;
import com.polus.fibicomp.globalentity.pojo.EntitySubOrgInfo;

@Repository
@Transactional
public class SubAwdOrgDAOImpl implements SubAwdOrgDAO {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	private static final Map<SubAwardOrgField, String> FIELD_MAPPINGS = Map.ofEntries(
			entry(SubAwardOrgField.organizationId, "organizationId"),
			entry(SubAwardOrgField.organizationTypeCode, "organizationTypeCode"),
			entry(SubAwardOrgField.samExpirationDate, "samExpirationDate"),
			entry(SubAwardOrgField.subAwdRiskAssmtDate, "subAwdRiskAssmtDate"),
			entry(SubAwardOrgField.feedStatusCode, "feedStatusCode"));

	@Override
	public Integer saveDetails(EntitySubOrgInfo entity) {
		hibernateTemplate.save(entity);
		return entity.getId();
	}

	@Override
	public void updateDetails(SubAwdOrgRequestDTO dto) {
		Map<SubAwardOrgField, Object> subAwardOrgFields = dto.getSubAwardOrgFields();

		if (subAwardOrgFields == null) {
			throw new IllegalArgumentException("subAwardOrgFields map is null.");
		}

		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();

		StringBuilder hqlQuery = new StringBuilder("UPDATE EntitySubOrgInfo e SET e.updatedBy = :updatedBy, e.updateTimestamp = :updateTimestamp");
		StringJoiner updates = new StringJoiner(", ");

		subAwardOrgFields.forEach((field, value) -> {
			String fieldName = FIELD_MAPPINGS.get(field);
			if (fieldName != null) {
				updates.add("e." + fieldName + " = :" + fieldName);
			} else {
				throw new IllegalArgumentException("Unknown field: " + field);
			}
		});

		hqlQuery.append(", ").append(updates.toString());
		hqlQuery.append(" WHERE e.entityId = :entityId");

		System.out.println("hqlQuery : " + hqlQuery.toString());
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("entityId", dto.getEntityId());
		query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
		query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());

		subAwardOrgFields.forEach((field, value) -> {
			if (field.equals(SubAwardOrgField.samExpirationDate)) {
				query.setParameter(FIELD_MAPPINGS.get(field), dto.getDateFromMap(field));
			} else if (field.equals(SubAwardOrgField.subAwdRiskAssmtDate)) {
				query.setParameter(FIELD_MAPPINGS.get(field), dto.getDateFromMap(field));
			} else {
				query.setParameter(FIELD_MAPPINGS.get(field), value);
			}
		});

		int updatedRows = query.executeUpdate();
		if (updatedRows == 0) {
			throw new EntityNotFoundException("Entity with ID " + dto.getEntityId() + " not found.");
		}
	}

}
