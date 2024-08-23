package com.polus.fibicomp.globalentity.dao;

import javax.persistence.Query;

import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.globalentity.dto.AddressDetailsRequestDTO;
import com.polus.fibicomp.globalentity.dto.IndustryDetailsRequestDTO;
import com.polus.fibicomp.globalentity.dto.OtherDetailsRequestDTO;
import com.polus.fibicomp.globalentity.dto.RegistrationDetailsRequestDTO;
import com.polus.fibicomp.globalentity.pojo.EntityForeignName;
import com.polus.fibicomp.globalentity.pojo.EntityIndustryClassification;
import com.polus.fibicomp.globalentity.pojo.EntityMailingAddress;
import com.polus.fibicomp.globalentity.pojo.EntityPriorName;
import com.polus.fibicomp.globalentity.pojo.EntityRegistration;

@Repository
@Transactional
public class CompanyDetailsDAOImpl implements CompanyDetailsDAO {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	@Override
	public int saveIndustryDetails(EntityIndustryClassification entity) {
		hibernateTemplate.save(entity);
		return entity.getEntityIndustryClassId();
	}

	@Override
	public void updateIndustryDetails(IndustryDetailsRequestDTO dto) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append(
				"UPDATE EntityIndustryClassification e SET e.updatedBy = :updatedBy, e.updateTimestamp = :updateTimestamp");
		hqlQuery.append(", e.industryCategoryId = :industryCategoryId");
		hqlQuery.append(" WHERE e.entityIndustryClassId = :entityIndustryClassId");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("entityIndustryClassId", dto.getEntityIndustryClassId());
		query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
		query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());
		query.setParameter("industryCategoryId", dto.getEntityIndustryCatId());
		query.executeUpdate();
	}

	@Override
	public int saveRegistrationDetails(EntityRegistration entity) {
		hibernateTemplate.save(entity);
		return entity.getEntityRegistrationId();
	}

	@Override
	public void updateRegistrationDetails(RegistrationDetailsRequestDTO dto) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append(
				"UPDATE EntityRegistration e SET e.updatedBy = :updatedBy, e.updateTimestamp = :updateTimestamp");
		if (dto.getRegTypeCode() != null) {
			hqlQuery.append(", e.regTypeCode = :regTypeCode");
		}
		if (dto.getRegNumber() != null) {
			hqlQuery.append(", e.regNumber = :regNumber");
		}
		if (dto.getIsActive() != null) {
			hqlQuery.append(", e.isActive = :isActive");
		}
		hqlQuery.append(" WHERE e.entityRegistrationId = :entityRegistrationId");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("entityRegistrationId", dto.getEntityRegistrationId());
		query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
		query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());
		if (dto.getRegTypeCode() != null) {
			query.setParameter("regTypeCode", dto.getRegTypeCode());
		}
		if (dto.getRegNumber() != null) {
			query.setParameter("regNumber", dto.getRegNumber());
		}
		if (dto.getIsActive() != null) {
			query.setParameter("isActive", dto.getIsActive());
		}
		query.executeUpdate();
	}

	@Override
	public int saveAdditionalAddresses(EntityMailingAddress entity) {
		hibernateTemplate.save(entity);
		return entity.getEntityMailingAddressId();
	}

	@Override
	public void updateAdditionalAddresses(AddressDetailsRequestDTO dto) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append(
				"UPDATE EntityMailingAddress e SET e.updatedBy = :updatedBy, e.updateTimestamp = :updateTimestamp");
		if (dto.getAddressTypeCode() != null) {
			hqlQuery.append(", e.addressTypeCode = :addressTypeCode");
		}
		if (dto.getAddressLine1() != null) {
			hqlQuery.append(", e.addressLine1 = :addressLine1");
		}
		if (dto.getAddressLine2() != null) {
			hqlQuery.append(", e.addressLine2 = :addressLine2");
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
		if (dto.getLocality() != null) {
			hqlQuery.append(", e.locality = :locality");
		}
		if (dto.getRegion() != null) {
			hqlQuery.append(", e.region = :region");
		}
		if (dto.getCounty() != null) {
			hqlQuery.append(", e.county = :county");
		}
		hqlQuery.append(" WHERE e.entityMailingAddressId = :entityMailingAddressId");
		Query query = session.createQuery(hqlQuery.toString());
		if (dto.getAddressTypeCode() != null) {
			query.setParameter("addressTypeCode", dto.getAddressTypeCode());
		}
		if (dto.getAddressLine1() != null) {
			query.setParameter("addressLine1", dto.getAddressLine1());
		}
		if (dto.getAddressLine2() != null) {
			query.setParameter("addressLine2", dto.getAddressLine2());
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
		if (dto.getLocality() != null) {
			query.setParameter("locality", dto.getLocality());
		}
		if (dto.getRegion() != null) {
			query.setParameter("region", dto.getRegion());
		}
		if (dto.getCounty() != null) {
			query.setParameter("county", dto.getCounty());
		}
		query.setParameter("entityMailingAddressId", dto.getEntityMailingAddressId());
		query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
		query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());
		query.executeUpdate();
	}

	@Override
	public void updateOtherDetails(OtherDetailsRequestDTO dto) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("UPDATE Entity e SET e.updatedBy = :updatedBy, e.updateTimestamp = :updateTimestamp");
		if (dto.getStartDate() != null) {
			hqlQuery.append(", e.startDate = :startDate");
		}
		if (dto.getIncorporationDate() != null) {
			hqlQuery.append(", e.incorporationDate = :incorporationDate");
		}
		if (dto.getCurrencyCode() != null) {
			hqlQuery.append(", e.currencyCode = :currencyCode");
		}
		if (dto.getIncorporatedIn() != null) {
			hqlQuery.append(", e.incorporatedIn = :incorporatedIn");
		}
		if (dto.getShortName() != null) {
			hqlQuery.append(", e.shortName = :shortName");
		}
		if (dto.getBusinessTypeCode() != null) {
			hqlQuery.append(", e.businessTypeCode = :businessTypeCode");
		}
		if (dto.getActivityText() != null) {
			hqlQuery.append(", e.activityText = :activityText");
		}
		if (dto.getCongressionalDistrict() != null) {
			hqlQuery.append(", e.congressionalDistrict = :congressionalDistrict");
		}
		if (dto.getFederalEmployerId() != null) {
			hqlQuery.append(", e.federalEmployerId = :federalEmployerId");
		}
		if (dto.getNumberOfEmployees() != null) {
			hqlQuery.append(", e.numberOfEmployees = :numberOfEmployees");
		}
		hqlQuery.append(" WHERE e.entityId = :entityId");
		Query query = session.createQuery(hqlQuery.toString());
		if (dto.getStartDate() != null) {
			query.setParameter("startDate", dto.getStartDate());
		}
		if (dto.getIncorporationDate() != null) {
			query.setParameter("incorporationDate", dto.getIncorporationDate());
		}
		if (dto.getCurrencyCode() != null) {
			query.setParameter("currencyCode", dto.getCurrencyCode());
		}
		if (dto.getIncorporatedIn() != null) {
			query.setParameter("incorporatedIn", dto.getIncorporatedIn());
		}
		if (dto.getShortName() != null) {
			query.setParameter("shortName", dto.getShortName());
		}
		if (dto.getBusinessTypeCode() != null) {
			query.setParameter("businessTypeCode", dto.getBusinessTypeCode());
		}
		if (dto.getActivityText() != null) {
			query.setParameter("activityText", dto.getActivityText());
		}
		if (dto.getCongressionalDistrict() != null) {
			query.setParameter("congressionalDistrict", dto.getCongressionalDistrict());
		}
		if (dto.getFederalEmployerId() != null) {
			query.setParameter("federalEmployerId", dto.getFederalEmployerId());
		}
		if (dto.getNumberOfEmployees() != null) {
			query.setParameter("numberOfEmployees", dto.getNumberOfEmployees());
		}
		query.setParameter("entityId", dto.getEntityId());
		query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
		query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());
		query.executeUpdate();
	}

	@Override
	public int savePriorName(EntityPriorName entity) {
		hibernateTemplate.save(entity);
		return entity.getId();
	}

	@Override
	public int saveForeignName(EntityForeignName entity) {
		hibernateTemplate.save(entity);
		return entity.getEntityId();
	}

	@Override
	public void updateIndustryDetailsPrimaryFlag(int primaryCatId, Integer entityId) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("UPDATE EntityIndustryClassification e ").append("SET e.isPrimary = CASE ")
				.append("WHEN e.entityId = :entityId AND e.isPrimary = 'Y' THEN 'N' ")
				.append("WHEN e.entityId = :entityId AND e.industryCategoryId = :primaryCatId THEN 'Y' ")
				.append("ELSE e.isPrimary END, ").append("e.updatedBy = :updatedBy, ")
				.append("e.updateTimestamp = :updateTimestamp");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("entityId", entityId);
		query.setParameter("primaryCatId", primaryCatId);
		query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
		query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());
		query.executeUpdate();
	}

}
