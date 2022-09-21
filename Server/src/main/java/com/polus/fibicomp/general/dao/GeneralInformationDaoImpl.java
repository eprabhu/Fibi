package com.polus.fibicomp.general.dao;

import java.io.Serializable;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.internal.SessionImpl;
import org.hibernate.transform.Transformers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.general.pojo.DynamicModuleConfig;
import com.polus.fibicomp.general.pojo.DynamicSectionConfig;
import com.polus.fibicomp.general.pojo.HelpText;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.vo.CommonVO;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "generalInformationDao")
public class GeneralInformationDaoImpl implements GeneralInformationDao {

	protected static Logger logger = LogManager.getLogger(GeneralInformationDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Override
	public Sponsor fetchSponsorData(String sponsorCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Sponsor> query = builder.createQuery(Sponsor.class);
		Root<Sponsor> sponsor = query.from(Sponsor.class);
		Predicate predicate1 = builder.equal(sponsor.get("sponsorCode"), sponsorCode);
		query.where(builder.and(predicate1));
		List<Sponsor> sponsorData = session.createQuery(query).list();
		return (sponsorData == null ? null : sponsorData.get(0));
	}

	@Override
	public String nextSponsorCode() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Sponsor> query = builder.createQuery(Sponsor.class);
		Root<Sponsor> root = query.from(Sponsor.class);
		query.where(builder.equal(root.get("sponsorCode"), "999999").not());
		query.orderBy(builder.desc(root.get("sponsorCode").as(Integer.class)));
		Sponsor sponsor = (Sponsor) session.createQuery(query).setMaxResults(1).uniqueResult();
		Integer maxSponsorCode = Integer.parseInt(sponsor.getSponsorCode());
		String nextSponsorCode = String.format("%06d", (maxSponsorCode + 1));
		return nextSponsorCode;
	}

	@Override
	public Integer insertSponsorData(Sponsor sponsor) {
		Serializable id = hibernateTemplate.save(sponsor);
		return Integer.parseInt(id.toString());
	}

	@Override
	public void updateSponsorData(Sponsor sponsor) {
		hibernateTemplate.update(sponsor);
	}

	@Override
	public void deleteSponsorData(Sponsor sponsor) {
		hibernateTemplate.delete(sponsor);
	}

	@Override
	public List<SponsorType> fetchSponsorTypeLookup() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SponsorType> query = builder.createQuery(SponsorType.class);
		Root<SponsorType> rootUnit = query.from(SponsorType.class);
		query.orderBy(builder.asc(rootUnit.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String syncPersonRole() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		String result = "data refreshed";
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_UNIT_WITH_CHILDREN ()}");
				statement.execute();
				statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_UNIT_WITH_CHILDREN ";
				String functionCall = "{call " + procedureName + "(?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.execute();
				statement.getObject(1);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		refresPersonRoleRT();
		return result;
	}

	private void refresPersonRoleRT() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call REFRESH_PERSON_ROLE_RT ()}");
				statement.execute();
				statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "REFRESH_PERSON_ROLE_RT ";
				String functionCall = "{call " + procedureName + "(?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.execute();
				statement.getObject(1);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@SuppressWarnings("deprecation")
	@Override
	public CommonVO getAllSponsors(CommonVO vo) {
		Integer pageNumber = vo.getPageNumber();
		Integer currentPage = vo.getCurrentPage();
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		String property3 = vo.getProperty3();
		String property4 = vo.getProperty4();
		String property5 = vo.getProperty5();
		String property6 = vo.getProperty6();
		String property7 = vo.getProperty7();
		Map<String, String> sort = vo.getSort();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria countCriteria = session.createCriteria(Sponsor.class);
		Conjunction and = Restrictions.conjunction();
		Criteria criteria = session.createCriteria(Sponsor.class);
		criteria.createAlias("sponsorType", "sponsorType");
		countCriteria.createAlias("sponsorType", "sponsorType");
		countCriteria.createAlias("unit", "unit", Criteria.LEFT_JOIN);
		criteria.createAlias("unit", "unit", Criteria.LEFT_JOIN);
		if (sort.isEmpty()) {
			criteria.addOrder(Order.asc("sponsorName")).addOrder(Order.asc("sponsorCode"));
		} else {
			for (Map.Entry<String, String> mapElement : sort.entrySet()) {
				if (mapElement.getValue().equalsIgnoreCase("desc")) {
					criteria.addOrder(Order.desc(mapElement.getKey()));
				} else {
					criteria.addOrder(Order.asc(mapElement.getKey()));
				}
			}
		}
		if (property1 != null && !property1.isEmpty()) {
			and.add(Restrictions.like("sponsorTypeCode", "%" + property1 + "%").ignoreCase());
		}
		if (property2 != null && !property2.isEmpty()) {
			and.add(Restrictions.like("sponsorName", "%" + property2 + "%").ignoreCase());
		}
		if (property3 != null && !property3.isEmpty()) {
			and.add(Restrictions.like("sponsorCode", "%" + property3 + "%").ignoreCase());
		}
		if (property4 != null && !property4.isEmpty()) {
			and.add(Restrictions.like("acronym", "%" + property4 + "%").ignoreCase());
		}
		if (property5 != null && !property5.isEmpty()) {
			and.add(Restrictions.like("sponsorLocation", "%" + property5 + "%").ignoreCase());
		}
		if (property6 != null && !property6.isEmpty()) {
			and.add(Restrictions.like("unitNumber", "%" + property6 + "%").ignoreCase());
		}
		if (property7 != null && !property7.isEmpty()) {
			and.add(Restrictions.like("sponsorGroup", "%" + property7 + "%").ignoreCase());
		}
		criteria.add(and);
		countCriteria.add(and);
		ProjectionList distinctProjectionList = Projections.projectionList();
		ProjectionList projectionList = Projections.projectionList();
		projectionList.add(Projections.distinct(distinctProjectionList.add(Projections.property("sponsorCode"), "sponsorCode")));
		projectionList.add(Projections.property("sponsorName"), "sponsorName");
		projectionList.add(Projections.property("sponsorType"), "sponsorType");
		projectionList.add(Projections.property("acronym"), "acronym");
		projectionList.add(Projections.property("unit"), "unit");
		projectionList.add(Projections.property("sponsorLocation"), "sponsorLocation");
		projectionList.add(Projections.property("countryCode"), "countryCode");
		projectionList.add(Projections.property("country"), "country");
		projectionList.add(Projections.property("sponsorGroup"), "sponsorGroup");
		criteria.setProjection(projectionList);
		criteria.setResultTransformer(Transformers.aliasToBean(Sponsor.class)); 
		countCriteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
		Long sponsorCount = (Long) countCriteria.setProjection(Projections.rowCount()).uniqueResult();
		logger.info("sponsorCount : " + sponsorCount);
		int count = pageNumber * (currentPage - 1);
		criteria.setFirstResult(count);
		criteria.setMaxResults(pageNumber);
		@SuppressWarnings("unchecked")
		List<Sponsor> sponsors = criteria.list();
		vo.setSponsors(sponsors);
		vo.setTotalSponsors(sponsorCount.intValue());
		return vo;
	}

	@Override
	public boolean evaluateRule(Integer moduleCode, Integer subModuleCode, String moduleItemKey, Integer ruleId, String logginPersonId, String updateUser, String subModuleItemKey) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call rule_evaluation(?,?,?,?,?,?,?,?)}");
				// Intentionally set 1st & 2nd argument repeatedly, we have an issue in the procedure that it is not getting the 1st argument
				// passed thru the procedure even it have the data. This is just a work around, need to find the root cause.
				statement.setInt(1, moduleCode);
				statement.setInt(2, moduleCode);
				statement.setInt(3, subModuleCode);
				statement.setString(4, moduleItemKey);
				statement.setInt(5, ruleId);
				statement.setString(6, logginPersonId);
				statement.setString(7, updateUser);
				statement.setString(8, subModuleItemKey);
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call rule_evaluation(?,?,?,?,?,?,?,?)}");
				statement.setInt(1, moduleCode);
				statement.setInt(2, subModuleCode);
				statement.setString(3, moduleItemKey);
				statement.setInt(4, ruleId);
				statement.setString(5, logginPersonId);
				statement.setString(6, updateUser);
				statement.setString(7, subModuleItemKey);
				statement.registerOutParameter(8, OracleTypes.CURSOR);
				statement.execute();
				rset = (ResultSet) statement.getObject(8);
			}
			while (rset.next()) {
				if ((rset.getString("RETURN_VALUE").toString()).equals("1")) {
					return true;
				}
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return false;
	}

	@Override
	public HelpText gethelpText(Integer moduleCode, Integer sectionCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<HelpText> query = builder.createQuery(HelpText.class);
		Root<HelpText> rootHelpText = query.from(HelpText.class);
		Predicate predicateModuleCode = builder.equal(rootHelpText.get("moduleCode"), moduleCode);
		Predicate predicateSectionCode = builder.equal(rootHelpText.get("sectionCode"), sectionCode);
		Predicate predicateIsActive = builder.equal(rootHelpText.get("isActive"), Constants.TRUE);
		query.where(builder.and(predicateModuleCode, predicateSectionCode, predicateIsActive));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public List<HelpText> gethelpTextBasedOnHelpTextId(Integer parentHelpTextId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<HelpText> query = builder.createQuery(HelpText.class);
		Root<HelpText> rootHelpText = query.from(HelpText.class);
		Predicate predicatehelpTextId = builder.equal(rootHelpText.get("parentHelpTextId"), parentHelpTextId);
		Predicate predicateIsActive = builder.equal(rootHelpText.get("isActive"), Constants.TRUE);
		query.where(builder.and(predicatehelpTextId, predicateIsActive));
		return session.createQuery(query).getResultList();
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<DynamicSectionConfig> getSectionConfiguration(String moduleCode) {				
		String hqlQuery = "select t1 from DynamicSectionConfig t1 inner join DynamicModuleConfig t2 on t2.moduleCode = t1.moduleCode where t1.moduleCode =:moduleCode and t2.isActive =:isActive ";			
		Query queryClaim =  hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryClaim.setParameter("moduleCode",moduleCode);
		queryClaim.setParameter("isActive",true);
		return queryClaim.getResultList();
	}

	@Override
	public List<DynamicModuleConfig> getModuleConfiguration() {
		return hibernateTemplate.loadAll(DynamicModuleConfig.class);
	}

}
