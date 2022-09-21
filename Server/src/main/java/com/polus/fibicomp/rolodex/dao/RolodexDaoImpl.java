package com.polus.fibicomp.rolodex.dao;

import java.util.List;
import java.util.Map;

import javax.persistence.Query;

import org.apache.commons.collections4.ListUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.pojo.Rolodex;
import com.polus.fibicomp.rolodex.vo.RolodexSearchResult;
import com.polus.fibicomp.rolodex.vo.RolodexVO;

@Transactional
@Service(value = "rolodexDao")
public class RolodexDaoImpl implements RolodexDao{

	protected static Logger logger = LogManager.getLogger(RolodexDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonService commonService;

	private static final String SPONSOR_COUNTRY_CODE = "sponsor.countryCode";

	@SuppressWarnings("unchecked")
	@Override
	public List<RolodexSearchResult> findRolodex(String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		final String likeCriteria = "%" + searchString.toUpperCase() + "%";
		Query query = session.createQuery("SELECT NEW com.polus.fibicomp.rolodex.vo.RolodexSearchResult(t.rolodexId, t.firstName, t.middleName, t.lastName, t.prefix) " +
                "FROM Rolodex t " +
                "WHERE UPPER(t.firstName) like :likeCriteria OR UPPER(t.lastName) like :likeCriteria OR UPPER(t.middleName) like :likeCriteria OR UPPER(t.prefix) like :likeCriteria OR (t.rolodexId) like :likeCriteria");
		query.setParameter("likeCriteria", likeCriteria);
		return ListUtils.emptyIfNull(query.setMaxResults(25).getResultList());
	}

	@Override
	public Rolodex getRolodexDetailById(Integer rolodexId) {
		return hibernateTemplate.get(Rolodex.class, rolodexId);
	}

	@Override
	public Rolodex saveOrUpdateRolodex(Rolodex rolodex) {
		try {
			hibernateTemplate.saveOrUpdate(rolodex);
		} catch (Exception e) {
			logger.info("Exception in saveorupdate : {} " , e.getMessage());
		}
		return rolodex;
	}

	@Override
	public void deleteRolodex(Rolodex rolodex) {
		hibernateTemplate.delete(rolodex);
	}

	@Override
	public RolodexVO getAllRolodexes(RolodexVO vo) {
		Integer pageNumber = vo.getPageNumber();
		Integer currentPage = vo.getCurrentPage();
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		String property3 = vo.getProperty3();
		String property4 = vo.getProperty4();
		String property5 = vo.getProperty5();
		Boolean property6 = vo.getProperty6();
		String property7 = vo.getProperty7();
		String property8 = vo.getProperty8();
		String property9 = vo.getProperty9();
		String property10 = vo.getProperty10();
		String property11 = vo.getProperty11();
		String property12 = vo.getProperty12();
		Map<String, String> sort = vo.getSort();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		@SuppressWarnings("deprecation")
		Criteria countCriteria = session.createCriteria(Rolodex.class);
		countCriteria.createAlias("sponsor", "sponsor", JoinType.LEFT_OUTER_JOIN);
		countCriteria.createAlias("organizations", "organization", JoinType.LEFT_OUTER_JOIN);
		Conjunction and = Restrictions.conjunction();
		Disjunction or = Restrictions.disjunction();
		@SuppressWarnings("deprecation")
		Criteria criteria = session.createCriteria(Rolodex.class);
		criteria.createAlias("sponsor", "sponsor", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("organizations", "organization", JoinType.LEFT_OUTER_JOIN);
		if (sort.isEmpty()) {
			criteria.addOrder(Order.desc("updateTimestamp")).addOrder(Order.desc("rolodexId"));
		} else {
			for (Map.Entry<String, String> mapElement : sort.entrySet()) {
				if (mapElement.getKey().equals(SPONSOR_COUNTRY_CODE) && mapElement.getValue().equals("desc")) {
					criteria.addOrder(Order.desc(mapElement.getKey()));
				} else if (mapElement.getKey().equals(SPONSOR_COUNTRY_CODE) && mapElement.getValue().equals("asc")) {
					criteria.addOrder(Order.asc(mapElement.getKey()));
				} else if (mapElement.getValue().equalsIgnoreCase("desc")) {
					criteria.addOrder(Order.desc(mapElement.getKey()));
				} else {
					criteria.addOrder(Order.asc(mapElement.getKey()));
				}
			}
		}
		if (property1 != null && !property1.isEmpty()) {
			and.add(Restrictions.like("lastName", "%" + property1 + "%").ignoreCase());
		}
		if (property2 != null && !property2.isEmpty()) {
			and.add(Restrictions.like("firstName", "%" + property2 + "%").ignoreCase());
		}
		if (property3 != null && !property3.isEmpty()) {
			and.add(Restrictions.like("middleName", "%" + property3 + "%").ignoreCase());
		}
		if (property4 != null && !property4.isEmpty()) {
			and.add(Restrictions.like("sponsorCode", "%" + property4 + "%").ignoreCase());
		}
		if (property5 != null && !property5.isEmpty()) {
			and.add(Restrictions.like("city", "%" + property5 + "%").ignoreCase());
		}
		if (property6 != null) {
			and.add(Restrictions.eq("active", property6));
		}
		if (property7 != null && !property7.isEmpty()) {
			or.add(Restrictions.like("organizationName", "%" + property7 + "%").ignoreCase());
			or.add(Restrictions.like("organization.organizationName", "%" + property7 + "%").ignoreCase());
		}
		if (property8 != null && !property8.isEmpty()) {
			and.add(Restrictions.like("state", "%" + property8 + "%").ignoreCase());
		}
		if (property9 != null && !property9.isEmpty()) {
			and.add(Restrictions.like("countryCode", "%" + property9 + "%").ignoreCase());
		}
		if (property10 != null && !property10.isEmpty()) {
			and.add(Restrictions.like("fullName", "%" + property10 + "%").ignoreCase());
		}
		if (property11 != null && !property11.isEmpty()) {
			and.add(Restrictions.like(SPONSOR_COUNTRY_CODE, "%" + property11 + "%").ignoreCase());
		}
		if (property12 != null && !property12.isEmpty()) {
			and.add(Restrictions.like("createUser", "%" + property12 + "%").ignoreCase());
		}
		criteria.add(and);
		criteria.add(or);
		criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
		countCriteria.add(and);
		countCriteria.add(or);
		countCriteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
		Integer rolodexCount = countCriteria.list().size();
		logger.info("rolodexCount : {} " , rolodexCount);
		int count = pageNumber * (currentPage - 1);
		criteria.setFirstResult(count);
		criteria.setMaxResults(pageNumber);
		@SuppressWarnings("unchecked")
		List<Rolodex> rolodexes = criteria.list();
		rolodexes.stream().filter(rolodex -> rolodex.getSponsorCode() != null).forEach(rolodex -> 
		rolodex.setSponsorName(commonService.getSponsorFormatBySponsorDetail(rolodex.getSponsor().getSponsorCode(), rolodex.getSponsor().getSponsorName(), rolodex.getSponsor().getAcronym())));
		vo.setRolodexes(rolodexes);
		vo.setTotalRolodex(rolodexCount);
		return vo;
	}

	@Override
	public boolean checkUniqueEmailAddress(String emailId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT COUNT(*) FROM Rolodex WHERE emailAddress = :emailAddress";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("emailAddress", emailId);
		Long count = (Long) query.getSingleResult();
		return count > 0;
	}

	@Override
	public boolean checkPersonEmailAddress(Integer rolodexId, String emailAddress) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT COUNT(*) FROM Rolodex WHERE rolodexId = :rolodexId and emailAddress = :emailAddress";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("rolodexId", rolodexId);
		query.setParameter("emailAddress", emailAddress);
		Long count = (Long) query.getSingleResult();
		return  count > 0;
	}

	@Override
	public Organization saveOrUpdateOrganization(Organization organization) {
		try {
			hibernateTemplate.saveOrUpdate(organization);
		} catch (Exception e) {
			logger.info("Exception in saveOrUpdateOrganization : {} " , e.getMessage());
		}
		return organization;
	}

	@Override
	public Country fetchCountryByCountryCode(String countryCode) {
		return hibernateTemplate.get(Country.class, countryCode);
	}

}
