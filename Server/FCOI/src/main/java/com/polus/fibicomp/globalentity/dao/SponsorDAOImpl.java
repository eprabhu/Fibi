package com.polus.fibicomp.globalentity.dao;

import javax.persistence.Query;

import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.globalentity.dto.SponsorRequestDTO;
import com.polus.fibicomp.globalentity.pojo.EntitySponsorInfo;

@Repository
@Transactional
public class SponsorDAOImpl implements SponsorDAO {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	@Override
	public int saveDetails(EntitySponsorInfo entity) {
		hibernateTemplate.save(entity);
		return entity.getId();
	}

	@Override
	public void updateDetails(SponsorRequestDTO dto) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("UPDATE EntitySponsorInfo e SET e.updatedBy = :updatedBy, e.updateTimestamp = :updateTimestamp");
		if (dto.getSponsorTypeCode() != null) {
			hqlQuery.append(", e.sponsorTypeCode = :sponsorTypeCode");
		}
		if (dto.getAcronym() != null) {
			hqlQuery.append(", e.acronym = :acronym");
		}
		hqlQuery.append(" WHERE e.id = :id");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("id", dto.getId());
		query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
		query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());
		if (dto.getSponsorTypeCode() != null) {
			query.setParameter("sponsorTypeCode", dto.getSponsorTypeCode());
		}
		if (dto.getAcronym() != null) {
			query.setParameter("acronym", dto.getAcronym());
		}
		query.executeUpdate();
	}

}
