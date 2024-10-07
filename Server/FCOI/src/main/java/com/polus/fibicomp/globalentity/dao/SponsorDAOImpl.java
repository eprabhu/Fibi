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
import com.polus.fibicomp.globalentity.dto.EntitySponsorField;
import com.polus.fibicomp.globalentity.dto.SponsorRequestDTO;
import com.polus.fibicomp.globalentity.pojo.EntitySponsorInfo;

@Repository
@Transactional
public class SponsorDAOImpl implements SponsorDAO {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	private static final Map<EntitySponsorField, String> FIELD_MAPPINGS = Map.ofEntries(
			entry(EntitySponsorField.acronym, "acronym"),
			entry(EntitySponsorField.sponsorTypeCode, "sponsorTypeCode"),
			entry(EntitySponsorField.feedStatusCode, "feedStatusCode"));

	@Override
	public Integer saveDetails(EntitySponsorInfo entity) {
		hibernateTemplate.save(entity);
		return entity.getId();
	}

	@Override
	public void updateDetails(SponsorRequestDTO dto) {
		Map<EntitySponsorField, Object> entitySponsorFields = dto.getEntitySponsorFields();

		if (entitySponsorFields == null) {
			throw new IllegalArgumentException("entitySponsorFields map is null or empty.");
		}

		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();

		StringBuilder hqlQuery = new StringBuilder("UPDATE EntitySponsorInfo e SET e.updatedBy = :updatedBy, e.updateTimestamp = :updateTimestamp");
		StringJoiner updates = new StringJoiner(", ");

		entitySponsorFields.forEach((field, value) -> {
			String fieldName = FIELD_MAPPINGS.get(field);
			if (fieldName != null) {
				updates.add("e." + fieldName + " = :" + fieldName);
			} else {
				throw new IllegalArgumentException("Unknown field: " + field);
			}
		});

		hqlQuery.append(", ").append(updates.toString());
		hqlQuery.append(" WHERE e.entityId = :entityId");

		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("entityId", dto.getEntityId());
		query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
		query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());

		entitySponsorFields.forEach((field, value) -> {
			query.setParameter(FIELD_MAPPINGS.get(field), value);
		});

		int updatedRows = query.executeUpdate();
		if (updatedRows == 0) {
			throw new EntityNotFoundException("Entity with ID " + dto.getEntityId() + " not found.");
		}
	}

}
