package com.polus.fibicomp.adminportal.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.polus.fibicomp.common.dao.CommonDao;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.LogicalExpression;
import org.hibernate.criterion.MatchMode;
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

import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.adminportal.dto.UnitHierarchy;
import com.polus.fibicomp.pojo.ActivityType;
import com.polus.fibicomp.adminportal.pojo.InstituteLARate;
import com.polus.fibicomp.adminportal.pojo.InstituteRate;
import com.polus.fibicomp.adminportal.pojo.RateClass;
import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.pojo.UnitAdministrator;
import com.polus.fibicomp.adminportal.pojo.UnitAdministratorType;
import com.polus.fibicomp.adminportal.vo.UnitHierarchyVO;
import com.polus.fibicomp.adminportal.vo.UnitVO;
import com.polus.fibicomp.committee.dao.CommitteeDao;
import com.polus.fibicomp.person.pojo.Person;

import oracle.jdbc.OracleTypes;

@SuppressWarnings("deprecation")
@Transactional
@Service(value = "unitHierarchyDao")
public class UnitHierarchyDaoImpl implements UnitHierarchyDao {

	protected static Logger logger = LogManager.getLogger(UnitHierarchyDaoImpl.class.getName());

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommitteeDao committeeDao;

	@Autowired
	private CommonDao commonDao;

	public List<UnitHierarchy> getUnitHierarchy(String unitNumber) {
		logger.info("-------- getUnitHierarchyDAO ---------");
		List<UnitHierarchy> unitHierarchyList = new ArrayList<>();
		List<HashMap<String, Object>> unitList = new ArrayList<HashMap<String, Object>>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement callstm = null;
		ResultSet resultSet = null;
		Set<String> rootUnits = new HashSet<>();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				callstm = connection.prepareCall("{call GET_UNIT_HIERARCHY()}");
				callstm.execute();
				resultSet = callstm.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_UNIT_HIERARCHY";
				String functionCall = "{call " + procedureName + "(?,?)}";
				callstm = connection.prepareCall(functionCall);
				callstm.registerOutParameter(1, OracleTypes.CURSOR);
				callstm.setString(2, unitNumber);
				callstm.execute();
				resultSet = (ResultSet) callstm.getObject(1);
			}
			while (resultSet.next()) {
				HashMap<String, Object> detailsField = new HashMap<String, Object>();
				detailsField.put("unit_number", resultSet.getString("unit_number"));
				detailsField.put("unit_name", resultSet.getString("unit_name"));
				detailsField.put("parent_unit_number", resultSet.getString("parent_unit_number"));
				detailsField.put("lvl", resultSet.getString("lvl"));
				unitList.add(detailsField);
			}
			if (unitList != null && !unitList.isEmpty()) {
				HashMap<String, UnitHierarchy> hmUnits = new HashMap<>();
				UnitHierarchy parentUnitHierarchy = new UnitHierarchy();
				for (int index = 0; index < unitList.size(); index++) {
					HashMap<String, Object> hmResult = unitList.get(index);
					UnitHierarchy unitHierarchy = new UnitHierarchy();
					unitHierarchy.setUnitName((String) hmResult.get("unit_name"));
					unitHierarchy.setUnitNumber((String) hmResult.get("unit_number"));
					unitHierarchy.setParentUnitNumber((String) hmResult.get("parent_unit_number"));
					if(!(unitHierarchy.getParentUnitNumber() != null)) {
						rootUnits.add(unitHierarchy.getUnitNumber());
					}
					if (!hmUnits.isEmpty()) {
						parentUnitHierarchy = hmUnits.get((String) hmResult.get("parent_unit_number"));
						if (parentUnitHierarchy != null) {
							ArrayList<UnitHierarchy> parentHierarchyList = parentUnitHierarchy.getChildUnits();
							if (parentHierarchyList == null) {
								parentHierarchyList = new ArrayList<UnitHierarchy>();
							}
							parentHierarchyList.add(unitHierarchy);
							parentUnitHierarchy.setChildUnits(parentHierarchyList);
						}
					}
					hmUnits.put((String) hmResult.get("unit_number"), unitHierarchy);
				}
				rootUnits.forEach(unit -> unitHierarchyList.add(hmUnits.get(unit)));
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return unitHierarchyList;
	}

	@Override
	public List<UnitAdministratorType> getUnitAdministratorTypesList() {
		logger.info("-------- getUnitAdministratorTypesListDAO ---------");
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(UnitAdministratorType.class);
		criteria.addOrder(Order.asc("description"));
		@SuppressWarnings("unchecked")
		List<UnitAdministratorType> unitAdministratorTypeList = criteria.list();
		return unitAdministratorTypeList;
	}

	@Override
	public String convertObjectToJSON(Object object) {
		String response = "";
		ObjectMapper mapper = new ObjectMapper();
		try {
			mapper.setVisibility(PropertyAccessor.FIELD, Visibility.ANY);
			response = mapper.writeValueAsString(object);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return response;
	}

	@Override
	public List<Unit> getUnitsList() {
		logger.info("-------- getUnitsListDAO ---------");
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(Unit.class);
		ProjectionList projectionList = Projections.projectionList();
        projectionList.add(Projections.groupProperty("unitNumber"),"unitNumber");
        projectionList.add(Projections.property("unitName"),"unitName");
        criteria.setProjection(projectionList);
        criteria.setResultTransformer(Transformers.aliasToBean(Unit.class)); 
		criteria.addOrder(Order.asc("unitNumber"));
		@SuppressWarnings("unchecked")
		List<Unit> unitList = criteria.list();
		return unitList;
	}

	public UnitVO getUnitDetails(UnitHierarchyVO vo) {
		logger.info("-------- getUnitDetailsDAO ---------");
		UnitVO unitVO = new UnitVO();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(Unit.class);
		logger.info("getUnitDetails unitNumber :" + vo.getUnitNumber());
		criteria.add(Restrictions.like("unitNumber", vo.getUnitNumber(), MatchMode.EXACT));
		@SuppressWarnings("unchecked")
		List<Unit> unitList = criteria.list();
		if (unitList != null && unitList.size() > 0) {
			Unit unit = unitList.get(0);
			if (unit.getParentUnitNumber() != null && !unit.getParentUnitNumber().isEmpty()) {
				Unit parentUnit = hibernateTemplate.get(Unit.class, unit.getParentUnitNumber());
				unit.setParentUnitName(parentUnit.getUnitName());
			}
			for (UnitAdministrator unitAdministrator : unit.getUnitAdministrators()) {
				Person personDetailsView = committeeDao.getPersonDetailsById(unitAdministrator.getPersonId());
				if (personDetailsView != null) {
					unitAdministrator.setFullName(personDetailsView.getFullName());
				}
			}
			if (unit.getOrganizationId() != null) {
				unit.setOrganizationName(commonDao.loadOrganizationDetails(unit.getOrganizationId()).getOrganizationName());
			}
			unitVO.setUnit(unit);
		}
		return unitVO;
	}

	@Override
	public String addNewUnit(Unit unit) {
		logger.info("-------- addNewUnitDAO ---------");
		logger.info("add_unitNumber :" + unit.getUnitNumber());
		hibernateTemplate.saveOrUpdate(unit);
		String response = "Units are added successfully"; 
		return convertObjectToJSON(response);
	}

	@Override
	public String addNewUnitAdministrator(UnitAdministrator unitAdministrator,String acType) {
		logger.info("-------- addNewunitAdministratorDAO ---------");
		if(acType.equalsIgnoreCase("I")){
			hibernateTemplate.saveOrUpdate(unitAdministrator);
		}else{
			updateUnitAdministrator(unitAdministrator);
		}
		String response = "Units are added successfully";
		return convertObjectToJSON(response);
	}

	@SuppressWarnings("rawtypes")
	private void updateUnitAdministrator(UnitAdministrator unitAdministrator) {
		Integer status = null;
		if(unitAdministrator.getOldPersonId()!=null && unitAdministrator.getOldUnitAdministratorTypeCode()!=null){
			
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession()
                .createSQLQuery("update UNIT_ADMINISTRATOR SET PERSON_ID=:personId,"
                		+ "UNIT_ADMINISTRATOR_TYPE_CODE =:typeCode WHERE PERSON_ID=:oldPersonId "
                		+ "AND UNIT_ADMINISTRATOR_TYPE_CODE =:oldTypeCode AND UNIT_NUMBER =:unitNumber");

		query.setString("personId", unitAdministrator.getPersonId());
		query.setString("typeCode", unitAdministrator.getUnitAdministratorTypeCode());
		query.setString("oldPersonId", unitAdministrator.getOldPersonId());
		query.setString("oldTypeCode", unitAdministrator.getOldUnitAdministratorTypeCode());
		query.setString("unitNumber", unitAdministrator.getUnitNumber());
		status = query.executeUpdate();
		if(status ==0){
			System.out.println("success");
		}else if(status ==1){
			System.out.println("failed");
		}
		}else{
			hibernateTemplate.saveOrUpdate(unitAdministrator);
		}
		
	}

	@Override
	public UnitAdministratorType getUnitAdministratorTypeByCode(String code) {
		logger.info("-------- getUnitAdministratorTypeByCodeDAO ---------");
		logger.info("getUnitAdministratorTypeByCode code:" + code);
		UnitAdministratorType administratorType = hibernateTemplate.get(UnitAdministratorType.class, code);
		return administratorType;
	}

	@Override
	public List<InstituteRate> getRates(String unitNumber) {
		logger.info("-------- getRatesDAO ---------");
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(InstituteRate.class);
		if (unitNumber != null) {
			logger.info("getRates UnitNumber:" + unitNumber);
			@SuppressWarnings("unused")
			Unit unitName = hibernateTemplate.get(Unit.class, unitNumber);
			criteria.add(Restrictions.like("unitNumber", unitNumber, MatchMode.EXACT));
			criteria.addOrder(Order.desc("updateTimestamp"));
		}
		@SuppressWarnings("unchecked")
		List<InstituteRate> instituteRateList = criteria.list();
		return instituteRateList;
	}

	@Override
	public List<RateClass> getAllRateClass() {
		logger.info("-------- getAllRateClassDAO ---------");
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(RateClass.class);
		criteria.add(Restrictions.conjunction().add(Restrictions.ne("rateClassTypeCode", "L"))
				.add(Restrictions.ne("rateClassTypeCode", "Y")));
		criteria.addOrder(Order.asc("description"));
		@SuppressWarnings("unchecked")
		List<RateClass> rateClassList = criteria.list();
		return rateClassList;
	}

	@Override
	public List<RateType> getAllRateTypes() {
		logger.info("-------- getAllRateTypesDAO ---------");
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(RateType.class);
		criteria.add(Restrictions.conjunction().add(Restrictions.ne("rateClassCode", "10"))
				.add(Restrictions.ne("rateClassCode", "11")).add(Restrictions.ne("rateClassCode", "12")));
		criteria.addOrder(Order.asc("description"));
		@SuppressWarnings("unchecked")
		List<RateType> rateTypeList = criteria.list();
		return rateTypeList;
	}

	@Override
	public String deleteRate(InstituteRate instituteRate) {
		logger.info("-------- deleteRatesDAO ---------");
		@SuppressWarnings("unused")
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		if (instituteRate != null) {
			logger.info("deleteRate UnitNumber:" + instituteRate.getUnitNumber());
			hibernateTemplate.delete(instituteRate);
			String response = "Success";
			return convertObjectToJSON(response);
		} else {
			logger.info("-------- deleteRatesDAO Error---------");
			logger.info("instituteRate:" + instituteRate);
			String response = "Error";
			return convertObjectToJSON(response);
		}
	}

	@Override
	public List<ActivityType> getAtivityTypeList() {
		logger.info("-------- getRatesDAO ---------");
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(ActivityType.class);
		criteria.addOrder(Order.asc("description"));
		@SuppressWarnings("unchecked")
		List<ActivityType> activityType = criteria.list();
		return activityType;
	}

	@Override
	public String addInstituteRate(InstituteRate instituteRate) {
 		logger.info("addInstituteRate UnitNumber:" + instituteRate.getUnitNumber());
		Long instituteId = instituteRate.getId();
		if (instituteRate.getId() != null && !instituteRate.getId().equals("")) {
			instituteRate.setId(instituteId);
			logger.info("addInstituteRate id:" + instituteId);
		} else {
			if (oracledb.equalsIgnoreCase("Y")) {
				String nextSequenceId = getNextSeq("SEQ_INSTITUTE_RATES_ID");
				logger.info("getNextSeq : " + Long.parseLong(nextSequenceId));
				instituteRate.setId(Long.parseLong(nextSequenceId));
			} else {
				String nextSequenceId = getNextSeqMySql("SELECT max(INSTITUTE_RATE_ID)+1 FROM INSTITUTE_RATES");
				if(nextSequenceId.equals("0")) {
					nextSequenceId="1";
				}
				logger.info("getNextSeq : " + Long.parseLong(nextSequenceId));
				instituteRate.setId(Long.parseLong(nextSequenceId));
			}
		}
		hibernateTemplate.saveOrUpdate(instituteRate);
		String addRateStatus = "Success";
		return convertObjectToJSON(addRateStatus);
	}

	private String getNextSeqMySql(String sql) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		Statement statement;
		int nextSeq = 0;
		try {
			statement = connection.createStatement();
			ResultSet rs = statement.executeQuery(sql);

			if (rs.next()) {
				nextSeq = rs.getInt(1);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return Integer.toString(nextSeq);

	}

	private String getNextSeq(String sequence) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		Statement statement;
		int nextSeq = 0;
		try {
			statement = connection.createStatement();
			ResultSet rs = statement.executeQuery("select +" + sequence + ".nextval from dual");

			if (rs.next()) {
				nextSeq = rs.getInt(1);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return Integer.toString(nextSeq);
	}

	@Override
	public String deleteUnitAdministrator(List<UnitAdministrator> unitAdministratorsList) {
		logger.info("-------- delete UnitAdministrator Dao ---------");
		@SuppressWarnings("unused")
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		if (unitAdministratorsList != null && !unitAdministratorsList.isEmpty()) {
			logger.info("unitAdministratorsList Count:" + unitAdministratorsList.size());
			hibernateTemplate.deleteAll(unitAdministratorsList);
			String response = "Success";
			return convertObjectToJSON(response);
		} else {
			logger.info("-------- delete UnitAdministrator Dao Error---------");
			logger.info("unitAdministratorsList:" + unitAdministratorsList);
			String response = "Error";
			return convertObjectToJSON(response);
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<RateType> getAllLARateTypes() {
		logger.info("-------- getAllRateTypesDAO ---------");
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(RateType.class);
		criteria.add(Restrictions.disjunction().add(Restrictions.like("rateClassCode", "10"))
				.add(Restrictions.like("rateClassCode", "11")).add(Restrictions.like("rateClassCode", "12")));
		List<RateType> rateTypeList = criteria.list();
		return rateTypeList;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<RateClass> getAllLARateClass() {
		logger.info("-------- getAll LA RateClassDAO ---------");
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(RateClass.class);
		Criterion first_condition = Restrictions.like("rateClassTypeCode", "L", MatchMode.EXACT);
		Criterion second_condition = Restrictions.like("rateClassTypeCode", "Y", MatchMode.EXACT);
		LogicalExpression orExp = Restrictions.or(first_condition, second_condition);
		criteria.add(orExp);
		List<RateClass> rateClassList = criteria.list();
		return rateClassList;
	}

	@Override
	public List<InstituteLARate> getLARates(String unitNUmber) {
		logger.info("-------- get La Rates Dao ---------");
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(InstituteLARate.class);
		@SuppressWarnings("unused")
		Unit unitName = hibernateTemplate.get(Unit.class, unitNUmber);
		criteria.add(Restrictions.like("unitNumber", unitNUmber, MatchMode.EXACT));
		criteria.addOrder(Order.desc("updateTimestamp"));
		@SuppressWarnings("unchecked")
		List<InstituteLARate> instituteLaRateList = criteria.list();
		return instituteLaRateList;
	}

	@Override
	public String addInstituteLARate(InstituteLARate instituteLARate) {
		logger.info("-------- add la rates Dao ---------");
		logger.info("addInstituteRate UnitNumber:" + instituteLARate.getUnitNumber());
		Long instituteLAId = instituteLARate.getId();
		logger.info("addInstituteRate instituteLAId:" + instituteLAId);
		if (instituteLARate.getId() != null && !instituteLARate.getId().equals("")) {
			instituteLARate.setId(instituteLAId);
			logger.info("addInstituteLARate id:" + instituteLAId);
		}
		logger.info("save or update instituteLARate ID:" + instituteLARate.getId());
		logger.info("save or update instituteLARate UnitNumber:" + instituteLARate.getUnitNumber());
		logger.info("save or update instituteLARate RateClassCode:" + instituteLARate.getRateClassCode());
		hibernateTemplate.saveOrUpdate(instituteLARate);
		String addRateStatus = "Success";
		return convertObjectToJSON(addRateStatus);
	}

	@Override
	public String deleteLARate(InstituteLARate instituteLARate) {
		logger.info("-------- delete LA RatesDAO ---------");
		if (instituteLARate != null) {
			logger.info("delete instituteLARate UnitNumber:" + instituteLARate.getUnitNumber());
			logger.info("delete instituteLARate RateClassCode:" + instituteLARate.getRateClassCode());
			hibernateTemplate.delete(instituteLARate);
			String response = "Success";
			return convertObjectToJSON(response);
		} else {
			logger.info("-------- delete LA RatesDAO Error---------");
			logger.info("instituteLARate:" + instituteLARate);
			String response = "Error";
			return convertObjectToJSON(response);
		}
	}

	@Override
	public String fetchParentUnitNumberByUnitNumber(String unitNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT parentUnitNumber FROM Unit WHERE unitNumber=:unitNumber";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("unitNumber", unitNumber);
		return query.getSingleResult();
	}

	@Override
	public void syncUnitWithChildrenAndPersonRole(String unitNumber, String childUnitNumber, Boolean parentUnitChanged) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call SYNC_UNIT_WITH_CHILDREN_PERSON_ROLE (?,?,?)}");
				statement.setString(1, unitNumber);
				statement.setString(2, childUnitNumber);
				statement.setBoolean(3, parentUnitChanged);
				statement.execute();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call SYNC_UNIT_WITH_CHILDREN_PERSON_ROLE (?,?,?,?)}");
				statement.setString(1, unitNumber);
				statement.setString(2, childUnitNumber);
				statement.setBoolean(3, parentUnitChanged);
				statement.registerOutParameter(4, OracleTypes.CURSOR);
				statement.execute();
			}
		} catch (Exception e) {
			logger.error("Error occured while syncUnitWithChildrenAndPersonRole : {}", e.getMessage());
		}
		finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				logger.error("Error occured while closing callable statement for syncUnitWithChildrenAndPersonRole : {}", e.getMessage());
			}
		}
	}
}
