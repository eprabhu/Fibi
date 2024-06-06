package com.polus.formbuilder.customdataelement.dao;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;

import com.polus.appcorelib.pojo.LookupWindow;
import com.polus.formbuilder.customdataelement.pojo.CustomData;
import com.polus.formbuilder.customdataelement.pojo.CustomDataElementOption;
import com.polus.formbuilder.customdataelement.pojo.CustomDataElements;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Query;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import jakarta.transaction.Transactional;

@Transactional
@Service
public class CustomDataElementDaoImpl implements CustomDataElementDao {

	protected static Logger logger = LogManager.getLogger(CustomDataElementDaoImpl.class.getName());

	private static final String MODULE_ITEM_KEY = "moduleItemKey";

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public CustomDataElements saveOrUpdateCustomElement(CustomDataElements customDataElement) {
		hibernateTemplate.saveOrUpdate(customDataElement);
		return customDataElement;
	}

	@Override
	public List<CustomDataElements> fetchAllCustomElements() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CustomDataElements> query = builder.createQuery(CustomDataElements.class);
		Root<CustomDataElements> customDataElement = query.from(CustomDataElements.class);
		query.orderBy(builder.desc(customDataElement.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public CustomDataElements fetchCustomElementById(Integer customElementId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CustomDataElements> query = builder.createQuery(CustomDataElements.class);
		Root<CustomDataElements> customDataElement = query.from(CustomDataElements.class);
		Predicate predicate1 = builder.equal(customDataElement.get("customElementId"), customElementId);
		query.where(builder.and(predicate1));
		List<CustomDataElements> results = session.createQuery(query).getResultList();
		if (results.isEmpty()) {
			return new CustomDataElements(); 
		} else {
			return results.get(0);
		}
	}

	@Override
	public CustomDataElementOption saveOrUpdateElementOptions(CustomDataElementOption elementOption) {
		hibernateTemplate.saveOrUpdate(elementOption);
		return elementOption;
	}

	@Override
	public CustomData saveOrUpdateCustomResponse(CustomData customResponse) {
		hibernateTemplate.saveOrUpdate(customResponse);
		return customResponse;
	}

	@Override
	public List<CustomDataElementOption> getCustomOptions(Integer customElementId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CustomDataElementOption> query = builder.createQuery(CustomDataElementOption.class);
		Root<CustomDataElementOption> options = query.from(CustomDataElementOption.class);
		Predicate predicate1 = builder.equal(options.get("customDataElementsId"), customElementId);
		query.where(builder.and(predicate1));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteCustomOption(CustomDataElementOption option) {
		hibernateTemplate.delete(option);
	}

	@Override
	public List<CustomData> getCustomDataAnswers(String moduleItemKey, Integer moduleCode,
			String moduleSubItemKey, Integer subModuleCode, Integer customElementId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CustomData> query = builder.createQuery(CustomData.class);
		Root<CustomData> rootCustomData = query.from(CustomData.class);
		Predicate predicateOne = builder.equal(rootCustomData.get("customDataElementsId"), customElementId);
		Predicate predicateTwo = builder.equal(rootCustomData.get("moduleItemCode"), moduleCode);
		Predicate predicateThree = builder.equal(rootCustomData.get(MODULE_ITEM_KEY), moduleItemKey);
		Predicate predicateFour = builder.equal(rootCustomData.get("moduleSubItemKey"), moduleSubItemKey);
		Predicate predicateFive = builder.equal(rootCustomData.get("moduleSubItemCode"), subModuleCode);
		query.where(builder.and(predicateOne, predicateTwo, predicateThree, predicateFour, predicateFive));
		query.select(builder.construct(CustomData.class, rootCustomData.get("customDataId"), rootCustomData.get("value"), rootCustomData.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteOptionResponse(Integer customDataId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CustomData> query = builder.createQuery(CustomData.class);
		Root<CustomData> customDataElement = query.from(CustomData.class);
		Predicate predicate1 = builder.equal(customDataElement.get("customDataId"), customDataId);
		query.where(builder.and(predicate1));
		List<CustomData> results = session.createQuery(query).getResultList();
		if (!results.isEmpty()) {
			hibernateTemplate.delete(results.get(0));
		}
	}

	@Override
	public List<LookupWindow> getSystemLookupByCustomType(String dataTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<LookupWindow> lookupWindow = builder.createQuery(LookupWindow.class);
		Root<LookupWindow> rootLookupWindow = lookupWindow.from(LookupWindow.class);
		Predicate predicate1 = builder.equal(rootLookupWindow.get("dataTypeCode"), dataTypeCode);
		lookupWindow.where(builder.and(predicate1));
		lookupWindow.orderBy(builder.asc(rootLookupWindow.get("description")));
		return session.createQuery(lookupWindow).getResultList();
	}

	@Override
	public boolean isCustomElementNameExist(String customElementName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder criteriaBuilder = session.getCriteriaBuilder();
		CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
		Root<CustomDataElements> root = criteriaQuery.from(CustomDataElements.class);
		criteriaQuery.select(criteriaBuilder.count(root));
		criteriaQuery.where(criteriaBuilder.equal(root.get("customElementName"), customElementName));
		TypedQuery<Long> query = session.createQuery(criteriaQuery);
		Long count = query.getSingleResult();
		return count > 0;
	}

	@Override
	public List<String> getUserDeffinedLookup() {
		String sql = "SELECT ARGUMENT_NAME FROM ARG_VALUE_LOOKUP GROUP BY ARGUMENT_NAME ORDER BY ARGUMENT_NAME ASC";
		Query query = entityManager.createNativeQuery(sql);
		return query.getResultList();
	}

	@Override
	public List<Object> getCustomDataOptions(Integer customElementId) {
		List<Object> options = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Object[]> criteriaQuery = builder.createQuery(Object[].class);
		Root<CustomDataElementOption> root = criteriaQuery.from(CustomDataElementOption.class);
		criteriaQuery.select(builder.array(root.get("customDataOptionId"), root.get("optionName")));
		criteriaQuery.where(builder.equal(root.get("customDataElementsId"), customElementId));
		List<Object[]> resultList = session.createQuery(criteriaQuery).getResultList();
		if (resultList != null && !resultList.isEmpty()) {
			resultList.forEach(obj -> {
				Map<String, String> map = new HashMap<>();
				map.put("customDataOptionId", obj[0].toString());
				map.put("optionName", obj[1].toString());
				options.add(map);
			});
		}
		return options;
	}

}
