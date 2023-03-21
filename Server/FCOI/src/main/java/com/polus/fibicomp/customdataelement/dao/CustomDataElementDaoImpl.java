package com.polus.fibicomp.customdataelement.dao;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import com.polus.fibicomp.pojo.Module;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.customdataelement.pojo.CustomData;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElementDataType;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElementOption;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElementUsage;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElements;
import com.polus.fibicomp.customdataelement.pojo.LookupWindow;

@Transactional
@Service(value = "customDataElememntDao")
public class CustomDataElementDaoImpl implements CustomDataElementDao {

	protected static Logger logger = LogManager.getLogger(CustomDataElementDaoImpl.class.getName());

	private static final String MODULE_CODE = "moduleCode";
	private static final String COLUMN_ID = "columnId";
	private static final String ACTIVE = "active";
	private static final String MODULE_ITEM_KEY = "moduleItemKey";

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public CustomDataElements saveOrUpdateCustomElement(CustomDataElements customDataElement) {
		try {
			hibernateTemplate.saveOrUpdate(customDataElement);
		} catch (Exception e) {
			logger.info("Exception in saveOrUpdateCustomElement : {}", e.getMessage());
		}
		return customDataElement;
	}

	@Override
	public List<CustomDataElements> fetchAllCustomElements() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CustomDataElements> query = builder.createQuery(CustomDataElements.class);
		Root<CustomDataElements> customDataElement = query.from(CustomDataElements.class);
		Predicate predicate1 = builder.equal(customDataElement.get("isLatestVesrion"), "Y");
		query.where(builder.and(predicate1));
		query.orderBy(builder.desc(customDataElement.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public CustomDataElements fetchCustomElementById(Integer customElementId) {
		CustomDataElements customDataElement = hibernateTemplate.get(CustomDataElements.class, customElementId);
		for (int index = 0; index < customDataElement.getCustomDataElementUsage().size(); index++) {
			customDataElement.getCustomDataElementUsage().get(index).setAcType("U");
		}
		return customDataElement;
	}

	@Override
	public void deleteCutsomElementUsage(CustomDataElementUsage customDataElementUsage) {
		hibernateTemplate.delete(customDataElementUsage);
	}

	public String activeDeactivateCustomElementById(Integer customElementId) {
		CustomDataElements customDataElement = fetchCustomElementById(customElementId);
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String flag = "Y";
		if (customDataElement.getIsActive().equals("Y")) {
			flag = "N";
		}
		Query query = session.createSQLQuery("UPDATE CUSTOM_DATA_ELEMENTS SET IS_ACTIVE =:active WHERE COLUMN_ID = :columnId");
		query.setParameter(ACTIVE, flag);
		query.setParameter(COLUMN_ID, customDataElement.getColumnId());
		int rowCount = query.executeUpdate();
		logger.info("Rows affected: {}", rowCount);
		return "Custom data deleted succesfully";
	}

	@Override
	public List<CustomDataElementDataType> getCustomElementDataTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CustomDataElementDataType> query = builder.createQuery(CustomDataElementDataType.class);
		Root<CustomDataElementDataType> rootUnit = query.from(CustomDataElementDataType.class);
		query.orderBy(builder.asc(rootUnit.get("description")));
		return session.createQuery(query).getResultList();
	}

	public Integer getNextVersionNumber(Integer columnId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createSQLQuery("SELECT MAX(COLUMN_VERSION_NUMBER)+1 FROM CUSTOM_DATA_ELEMENTS WHERE COLUMN_ID = :columnId");
		query.setParameter(COLUMN_ID, columnId);
		return Integer.parseInt(query.getSingleResult().toString());
	}

	public void updateLatestFlag(Integer columnId, Integer customElementId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createSQLQuery(
				"UPDATE CUSTOM_DATA_ELEMENTS SET IS_LATEST_VERSION ='N' WHERE COLUMN_ID = :columnId AND CUSTOM_DATA_ELEMENTS_ID <>:customElementId ");
		query.setParameter(COLUMN_ID, columnId);
		query.setParameter("customElementId", customElementId);
		int rowCount = query.executeUpdate();
		logger.info("Rows affected: {}", rowCount);
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
	public List<CustomDataElementUsage> getApplicableCustomElement(Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
//		String hqlQuery = "FROM CustomDataElementUsage T JOIN FETCH T.customDataElement WHERE T.moduleCode = :moduleCode";
//		Query query = session.createQuery(hqlQuery);
//		query.setParameter("moduleCode", moduleCode);
//		return query.getResultList();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CustomDataElementUsage> query = builder.createQuery(CustomDataElementUsage.class);
		Root<CustomDataElementUsage> customDataUsage = query.from(CustomDataElementUsage.class);
		Predicate predicate1 = builder.equal(customDataUsage.get(MODULE_CODE), moduleCode);
		query.where(builder.and(predicate1));
		query.orderBy(builder.asc(customDataUsage.get("orderNumber")));
		query.select(customDataUsage);
		return session.createQuery(query).getResultList();
	}

	@Override
	public Integer getNextColumnId() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createSQLQuery("SELECT ifnull(MAX(COLUMN_ID),0)+1 AS columnId FROM CUSTOM_DATA_ELEMENTS");
		return Integer.parseInt(query.getSingleResult().toString());
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

	@SuppressWarnings("unchecked")
	@Override
	public List<String> getCustomDataOptions(Integer customElementId) {
		List<String> options = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT T.optionName FROM CustomDataElementOption T WHERE T.customDataElementsId = :customElementId";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("customElementId", customElementId);
		if (query.getResultList() != null && !query.getResultList().isEmpty()) {
			options = query.getResultList();
		}
		return options;
	}

	@Override
	public List<CustomData> getCustomDataAnswers(Integer moduleCode, String moduleItemKey, Integer customElementId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CustomData> query = builder.createQuery(CustomData.class);
		Root<CustomData> rootCustomData = query.from(CustomData.class);
		Predicate predicateOne = builder.equal(rootCustomData.get("customDataElementsId"), customElementId);
		Predicate predicateTwo = builder.equal(rootCustomData.get("moduleItemCode"), moduleCode);
		Predicate predicateThree = builder.equal(rootCustomData.get(MODULE_ITEM_KEY), moduleItemKey);
		query.where(builder.and(predicateOne, predicateTwo, predicateThree));
		query.select(builder.construct(CustomData.class, rootCustomData.get("customDataId"), rootCustomData.get("value"), rootCustomData.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<CustomData> getAllCustomDataAnswers(Integer moduleCode, String moduleItemKey, Integer customElementId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CustomData> query = builder.createQuery(CustomData.class);
		Root<CustomData> rootCustomData = query.from(CustomData.class);
		Predicate predicateOne = builder.equal(rootCustomData.get("customDataElementsId"), customElementId);
		Predicate predicateTwo = builder.equal(rootCustomData.get("moduleItemCode"), moduleCode);
		Predicate predicateThree = builder.equal(rootCustomData.get(MODULE_ITEM_KEY), moduleItemKey);
		query.where(builder.and(predicateOne, predicateTwo, predicateThree));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteOptionResponse(Integer customDataId) {
		hibernateTemplate.delete(hibernateTemplate.load(CustomData.class, customDataId));
	}

	@Override
	public Integer getModuleItemStatus(Integer moduleCode, String moduleItemKey) {
		String query = "SELECT STATUS_CODE FROM ";
		String tableName = null;
		String moduleItemName = null;
		if (moduleCode.equals(Constants.MODULE_CODE_DEVELOPMENT_PROPOSAL)) {
			tableName = "EPS_PROPOSAL";
			moduleItemName = "PROPOSAL_ID";
		} else if (moduleCode.equals(Constants.MODULE_CODE_AWARD)) {
			query = "SELECT WORKFLOW_AWARD_STATUS_CODE FROM ";
			tableName = "AWARD";
			moduleItemName = "AWARD_ID";
		}
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder resultQuery = new StringBuilder(query).append(tableName).append(" WHERE ").append(moduleItemName).append(" = :moduleItemKey");
		Query sqlQuery = session.createSQLQuery(resultQuery.toString());
		sqlQuery.setParameter("moduleItemKey", moduleItemKey);
		return Integer.parseInt(sqlQuery.getSingleResult().toString());
	}

	@Override
	public Integer getAnswerVersion(Integer columnId, Integer moduleCode, String moduleItemKey) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Integer> criteria = builder.createQuery( Integer.class );
			Root<CustomData> customData = criteria.from( CustomData.class );
			criteria.select(builder.max(customData.get("versionNumber")));
			Predicate predicate = builder.and( builder.equal(customData.get(COLUMN_ID), columnId ),
				    			builder.equal( customData.get("moduleItemCode"), moduleCode),
				    			builder.equal( customData.get("moduleItemKey"), moduleItemKey) );
			criteria.where(predicate);
			return session.createQuery(criteria).getSingleResult();
		} catch (Exception ex) {
			return null;
		}
	}

	@Override
	public Integer getAnsweredCustomElementId(Integer columnId, String moduleItemKey, Integer answerVersion, Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT distinct (customDataElementsId) FROM CustomData WHERE moduleItemKey = :moduleItemKey and columnId = :columnId and versionNumber = :answerVersion and moduleItemCode = :moduleCode";
		Query query = session.createQuery(hqlQuery);
		query.setParameter(COLUMN_ID, columnId);
		query.setParameter(MODULE_ITEM_KEY, moduleItemKey);
		query.setParameter("answerVersion", answerVersion);
		query.setParameter(MODULE_CODE, moduleCode);
		return Integer.parseInt(query.getSingleResult().toString());
	}

	@Override
	public List<CustomData> fetchCustomDataByParams(Integer moduleCode, Integer moduleItemKey, Integer subModuleCode, String subModuleItemKey) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CustomData> query = builder.createQuery(CustomData.class);
		Root<CustomData> rootCustomData = query.from(CustomData.class);
		Predicate predicateModuleItemCode = builder.equal(rootCustomData.get("moduleItemCode"), moduleCode);
		Predicate predicateModuleItemKey = builder.equal(rootCustomData.get(MODULE_ITEM_KEY), moduleItemKey);
		Predicate predicateSubModuleItemCode = builder.equal(rootCustomData.get("moduleSubItemCode"), subModuleCode);
		Predicate predicateSubModuleItemKey = builder.equal(rootCustomData.get("moduleSubItemKey"), subModuleItemKey);
		query.where(builder.and(predicateModuleItemCode, predicateModuleItemKey, predicateSubModuleItemCode, predicateSubModuleItemKey));
		return session.createQuery(query).getResultList();
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
		String hqlQuery = "SELECT COUNT(*) FROM CustomDataElements C WHERE C.customElementName = :customElementName";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("customElementName", customElementName);
		Long count = (Long) query.getSingleResult();
		return count > 0;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<CustomDataElementUsage> getCommonCustomElements(Integer subModuleCode, Integer baseModuleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("SELECT T1 FROM CustomDataElementUsage T1\r\n" + 
				"LEFT JOIN CustomDataElementUsage T2 ON T1.customDataElement.customElementId = T2.customDataElement.customElementId\r\n" + 
				"WHERE T2.moduleCode = :baseModuleCode AND T1.moduleCode = :subModuleCode AND T2.customDataElement.customElementId IS NOT NULL");
		query.setParameter("baseModuleCode", baseModuleCode);
		query.setParameter("subModuleCode", subModuleCode);
		return query.getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public CustomDataElements fetchCustomDataElementDetail(String customElementName) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("FROM CustomDataElements t1 WHERE t1.customElementName=:customElementName and t1.columnVersionNumber in (select MAX(t2.columnVersionNumber) from CustomDataElements t2 where t2.customElementName= t1.customElementName)");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("customElementName", customElementName);
			return ((org.hibernate.query.Query<CustomDataElements>) query).uniqueResult();
		} catch (Exception e) {
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public CustomData getCustomDataValue(Integer moduleCode, String moduleItemKey, Integer customDataElementsId) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("FROM CustomData t1 WHERE t1.moduleItemCode=:moduleItemCode and t1.moduleItemKey =:moduleItemKey and t1.customDataElementsId =:customDataElementsId");
			hqlQuery.append(" and t1.versionNumber in (select MAX(t2.versionNumber) from CustomData t2 where t2.moduleItemCode= t1.moduleItemCode and t2.moduleItemKey = t1.moduleItemKey and t2.customDataElementsId = t1.customDataElementsId)");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("moduleItemCode", moduleCode);
			query.setParameter(MODULE_ITEM_KEY, moduleItemKey);
			query.setParameter("customDataElementsId", customDataElementsId);
			return ((org.hibernate.query.Query<CustomData>) query).uniqueResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<CustomDataElements> getCustomElementByModuleCode(Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT custElement FROM CustomDataElementUsage T JOIN  WHERE " +
					"T.moduleCode = :moduleCode ORDER BY T.orderNumber";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("moduleCode", moduleCode);
		return query.getResultList();
	}

	@Override
	public void saveCustomElementOrderNumber(Integer customElementId, Integer orderNumber, Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "UPDATE CustomDataElementUsage cusEle SET cusEle.orderNumber = :orderNumber " +
				"WHERE cusEle.customDataElement.customElementId = :customElementId AND cusEle.moduleCode = :moduleCode";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("customElementId", customElementId);
		query.setParameter("orderNumber", orderNumber);
		query.setParameter("moduleCode", moduleCode);
		query.executeUpdate();
	}

	@Override
	public void updateCustomElementRequired(Integer elementId, Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("UPDATE CustomDataElementUsage usage SET usage.isRequired = " +
				"CASE usage.isRequired WHEN 'Y' THEN 'N' ELSE 'Y' END WHERE usage.moduleCode = :moduleCode AND " +
				"usage.customDataElement.customElementId = :customElementId");
		query.setParameter("moduleCode", moduleCode);
		query.setParameter("customElementId", elementId);
		query.executeUpdate();
	}

	@Override
	public int findLargestCusElementOrderNumber(Integer moduleCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query query = session.createQuery("SELECT IFNULL(MAX(usage.orderNumber),0)+1 FROM CustomDataElementUsage usage " +
					"WHERE usage.moduleCode = :moduleCode");
			query.setParameter("moduleCode", moduleCode);
			return (Integer) query.getSingleResult();
		} catch (Exception e) {
			logger.error("findLargestCusElementOrderNumber {}", e.getMessage());
		}
		return 0;
	}

	@Override
	public List<Module> getCusElementModules() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("SELECT usage.module FROM CustomDataElementUsage usage GROUP BY usage.moduleCode");
		return query.getResultList();
	}
}
