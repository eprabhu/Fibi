package com.polus.fibicomp.claims.claimsIntegration.ics.dao;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.claims.claimsIntegration.ics.pojo.IcsStudentTravelDetail;
import com.polus.fibicomp.pojo.Country;

@Transactional
@Service(value = "icsDao")
public class IcsDaoImpl implements IcsDao {

	protected static Logger logger = LogManager.getLogger(IcsDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public IcsStudentTravelDetail saveOrUpdateClaimStudentTravelDetail(IcsStudentTravelDetail claimStudentTravelDetail) {
		try {
			hibernateTemplate.saveOrUpdate(claimStudentTravelDetail);
		} catch (Exception e) {
			logger.info("Error ocuured in saveOrUpdateClaimStudentTravelDetail {}", e.getMessage());
		}
		return claimStudentTravelDetail;
	}

	@Override
	public Country fetchCountryCodeByCountryName(String countryName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Country> query = builder.createQuery(Country.class);
		Root<Country> rootCountry = query.from(Country.class);
		query.where(builder.and(builder.equal(rootCountry.get("countryName"), countryName)));
		return session.createQuery(query).uniqueResult();
	}

}
