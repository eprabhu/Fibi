package com.polus.fibicomp.globalentity.dao;

import javax.persistence.Query;

import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.globalentity.dto.SubAwdOrgRequestDTO;
import com.polus.fibicomp.globalentity.pojo.EntitySubOrgInfo;

@Repository
@Transactional
public class SubAwdOrgDAOImpl implements SubAwdOrgDAO {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	@Override
	public int saveDetails(EntitySubOrgInfo entity) {
		hibernateTemplate.save(entity);
		return entity.getId();
	}

	@Override
	public void updateDetails(SubAwdOrgRequestDTO dto) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("UPDATE EntitySubOrgInfo e SET e.updatedBy = :updatedBy, e.updateTimestamp = :updateTimestamp");
		if (dto.getOrganizationId() != null) {
			hqlQuery.append(", e.organizationId = :organizationId");
		}
		if (dto.getOrganizationTypeCode() != null) {
			hqlQuery.append(", e.organizationTypeCode = :organizationTypeCode");
		}
		if (dto.getSamExpirationDate() != null) {
			hqlQuery.append(", e.samExpirationDate = :samExpirationDate");
		}
		if (dto.getSubAwdRiskAssmtDate() != null) {
			hqlQuery.append(", e.subAwdRiskAssmtDate = :subAwdRiskAssmtDate");
		}
		hqlQuery.append(" WHERE e.entityId = :entityId");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("entityId", dto.getEntityId());
		query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
		query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());
		if (dto.getOrganizationId() != null) {
			query.setParameter("organizationId", dto.getOrganizationId());
		}
		if (dto.getOrganizationTypeCode() != null) {
			query.setParameter("organizationTypeCode", dto.getOrganizationTypeCode());
		}
		if (dto.getSamExpirationDate() != null) {
			query.setParameter("samExpirationDate", dto.getSamExpirationDate());
		}
		if (dto.getSubAwdRiskAssmtDate() != null) {
			query.setParameter("subAwdRiskAssmtDate", dto.getSubAwdRiskAssmtDate());
		}
		query.executeUpdate();
	}

}
