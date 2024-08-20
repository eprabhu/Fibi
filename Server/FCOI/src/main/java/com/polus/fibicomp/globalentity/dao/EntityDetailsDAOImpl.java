package com.polus.fibicomp.globalentity.dao;

import javax.persistence.Query;

import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.globalentity.dto.EntityRequestDTO;
import com.polus.fibicomp.globalentity.pojo.GlobalEntity;

@Repository
@Transactional
public class EntityDetailsDAOImpl implements EntityDetailsDAO {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	@Override
	public int createEntity(GlobalEntity entity) {
		hibernateTemplate.save(entity);
		return entity.getEntityId();
	}

	@Override
	public void updateEntity(EntityRequestDTO dto) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("UPDATE GlobalEntity e SET e.updatedBy = :updatedBy, e.updateTimestamp = :updateTimestamp");
		if (dto.getPrimaryName() != null) {
			hqlQuery.append(", e.primaryName = :primaryName");
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
		hqlQuery.append(" WHERE e.entityId = :entityId");
		Query query = session.createQuery(hqlQuery.toString());
		if (dto.getPrimaryName() != null) {
			query.setParameter("primaryName", dto.getPrimaryName());
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
		query.setParameter("entityId", dto.getEntityId());
		query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
		query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());
		query.executeUpdate();
	}

	@Override
	public GlobalEntity fetchEntityDetails(Integer entityId) {
		return hibernateTemplate.get(GlobalEntity.class, entityId);
	}

}
