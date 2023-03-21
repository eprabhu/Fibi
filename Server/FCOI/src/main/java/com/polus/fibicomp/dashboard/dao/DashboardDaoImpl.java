package com.polus.fibicomp.dashboard.dao;

import java.math.BigDecimal;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.coi.dto.COIFinancialEntityDto;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.vo.*;
import com.polus.fibicomp.evaluation.dao.EvaluationDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.negotiation.pojo.Negotiations;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.pojo.FileType;
import com.polus.fibicomp.pojo.ParameterBo;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.servicerequest.dao.ServiceRequestDao;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.view.AwardView;
import com.polus.fibicomp.view.DisclosureView;
import com.polus.fibicomp.vo.CommonVO;

import oracle.jdbc.OracleTypes;

@SuppressWarnings("deprecation")
@Transactional
@Service(value = "dashboardDao")
@PropertySource("classpath:application.properties")
public class DashboardDaoImpl implements DashboardDao {

	protected static Logger logger =  LogManager.getLogger(DashboardDaoImpl.class.getName());

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private ProposalModuleDao proposalModuleDao;

	@Autowired
	private ProposalDao proposalDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private ServiceRequestDao serviceRequestDao;

	@Autowired
	private EvaluationDao evaluationDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private CommonService commonService;

	@Override
	public DashBoardProfile getDashBoardDataForDisclosures(CommonVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		Integer pageNumber = vo.getPageNumber();
		String sortBy = vo.getSortBy();
		String reverse = vo.getReverse();
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		String property3 = vo.getProperty3();
		String property4 = vo.getProperty4();
		Integer currentPage = vo.getCurrentPage();
		String personId = vo.getPersonId();

		Conjunction and = Restrictions.conjunction();
		try {
			logger.info("----------- getDashBoardDataForDisclosures ------------");
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Criteria searchCriteria = session.createCriteria(DisclosureView.class);
			Criteria countCriteria = session.createCriteria(DisclosureView.class);
			if (sortBy.isEmpty() || reverse.isEmpty()) {
				searchCriteria.addOrder(Order.desc("updateTimeStamp"));
			} else {
				if (reverse.equals("DESC")) {
					searchCriteria.addOrder(Order.desc(sortBy));
				} else {
					searchCriteria.addOrder(Order.asc(sortBy));
				}
			}
			if (property1 != null && !property1.isEmpty()) {
				and.add(Restrictions.like("coiDisclosureNumber", "%" + property1 + "%").ignoreCase());
			}
			if (property2 != null && !property2.isEmpty()) {
				and.add(Restrictions.like("fullName", "%" + property2 + "%").ignoreCase());
			}
			if (property3 != null && !property3.isEmpty()) {
				and.add(Restrictions.like("disclosureDisposition", "%" + property3 + "%").ignoreCase());
			}
			if (property4 != null && !property4.isEmpty()) {
				and.add(Restrictions.like("moduleItemKey", "%" + property4 + "%").ignoreCase());
			}
			if (personId != null && !personId.isEmpty()) {
				searchCriteria.add(Restrictions.eq("personId", personId));
				countCriteria.add(Restrictions.eq("personId", personId));
			}

			searchCriteria.add(and);
			countCriteria.add(and);

			Long dashboardCount = (Long) countCriteria.setProjection(Projections.rowCount()).uniqueResult();
			logger.info("dashboardCount : " + dashboardCount);
			dashBoardProfile.setTotalServiceRequest(dashboardCount.intValue());

			int count = pageNumber * (currentPage - 1);
			searchCriteria.setFirstResult(count);
			searchCriteria.setMaxResults(pageNumber);
			@SuppressWarnings("unchecked")
			List<DisclosureView> disclosures = searchCriteria.list();
			dashBoardProfile.setDisclosureViews(disclosures);
		} catch (Exception e) {
			logger.error("Error in method getDashBoardDataForDisclosures");
			e.printStackTrace();
		}
		return dashBoardProfile;
	}

	@SuppressWarnings("rawtypes")
	public String getSponsorHierarchy(String sponsorCode) {
		if (areAllSponsorsMultiPi()) {
			return Constants.NIH_MULTIPLE_PI_HIERARCHY;
		}
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		for (String hierarchyName : getRoleHierarchies()) {
			Query countQuery = session.createSQLQuery(
					"SELECT COUNT(1) FROM SPONSOR_HIERARCHY S WHERE SPONSOR_CODE=:sponsorCode AND HIERARCHY_NAME=:hierarchyName");		
			countQuery.setString("sponsorCode", sponsorCode);
			countQuery.setString("hierarchyName", hierarchyName);
			BigDecimal count = (BigDecimal) countQuery.uniqueResult();
			if (count.intValue() > 0) {
				return hierarchyName;
			}
		}
		return Constants.DEFAULT_SPONSOR_HIERARCHY_NAME;
	}

	public Boolean areAllSponsorsMultiPi() {
		Boolean isMultiPI = false;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(ParameterBo.class);
		criteria.add(Restrictions.eq("namespaceCode", Constants.MODULE_NAMESPACE_PROPOSAL_DEVELOPMENT));
		criteria.add(Restrictions.eq("componentCode", Constants.DOCUMENT_COMPONENT));
		criteria.add(Restrictions.eq("name", Constants.ALL_SPONSOR_HIERARCHY_NIH_MULTI_PI));
		criteria.add(Restrictions.eq("applicationId", Constants.KC));
		ParameterBo parameterBo = (ParameterBo) criteria.uniqueResult();
		String value = parameterBo != null ? parameterBo.getValue() : null;
		if (value == null) {
			isMultiPI = false;
		} else if (value.equalsIgnoreCase("N")) {
			isMultiPI = false;
		} else {
			isMultiPI = true;
		}
		return isMultiPI;
	}

	protected Collection<String> getRoleHierarchies() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(ParameterBo.class);
		criteria.add(Restrictions.eq("namespaceCode", Constants.KC_GENERIC_PARAMETER_NAMESPACE));
		criteria.add(Restrictions.eq("componentCode", Constants.KC_ALL_PARAMETER_DETAIL_TYPE_CODE));
		criteria.add(Restrictions.eq("name", Constants.SPONSOR_HIERARCHIES_PARM));
		criteria.add(Restrictions.eq("applicationId", Constants.KC));
		ParameterBo parameterBo = (ParameterBo) criteria.uniqueResult();
		String strValues = parameterBo.getValue();
		if (strValues == null || StringUtils.isBlank(strValues)) {
			return Collections.emptyList();
		}
		final Collection<String> values = new ArrayList<String>();
		for (String value : strValues.split(",")) {
			values.add(value.trim());
		}

		return Collections.unmodifiableCollection(values);
	}

	@Override
	public DashBoardProfile getDashBoardDataForCommittee(CommonVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		/*
		 * Integer pageNumber = vo.getPageNumber(); String sortBy = vo.getSortBy();
		 * String reverse = vo.getReverse(); String property1 = vo.getProperty1();
		 * String property2 = vo.getProperty2(); String property3 = vo.getProperty3();
		 * String property4 = vo.getProperty4(); Integer currentPage =
		 * vo.getCurrentPage();
		 * 
		 * Conjunction and = Restrictions.conjunction(); try {
		 * logger.info("----------- getDashBoardDataForCommittee ------------"); Session
		 * session = hibernateTemplate.getSessionFactory().getCurrentSession(); Criteria
		 * searchCriteria = session.createCriteria(Committee.class); Criteria
		 * countCriteria = session.createCriteria(Committee.class); if (sortBy.isEmpty()
		 * || reverse.isEmpty()) {
		 * searchCriteria.addOrder(Order.desc("updateTimestamp")); } else { if
		 * (reverse.equals("DESC")) { searchCriteria.addOrder(Order.desc(sortBy)); }
		 * else { searchCriteria.addOrder(Order.asc(sortBy)); } } if (property1 != null
		 * && !property1.isEmpty()) { and.add(Restrictions.like("committeeId", "%" +
		 * property1 + "%").ignoreCase()); } if (property2 != null &&
		 * !property2.isEmpty()) { and.add(Restrictions.like("committeeName", "%" +
		 * property2 + "%").ignoreCase()); } if (property3 != null &&
		 * !property3.isEmpty()) { and.add(Restrictions.like("homeUnitNumber", "%" +
		 * property3 + "%").ignoreCase()); } if (property4 != null &&
		 * !property4.isEmpty()) { and.add(Restrictions.like("homeUnitName", "%" +
		 * property4 + "%").ignoreCase()); }
		 * 
		 * searchCriteria.add(and); ProjectionList projList =
		 * Projections.projectionList();
		 * projList.add(Projections.property("committeeId"), "committeeId");
		 * projList.add(Projections.property("committeeName"), "committeeName");
		 * projList.add(Projections.property("homeUnitNumber"), "homeUnitNumber");
		 * projList.add(Projections.property("homeUnitName"), "homeUnitName");
		 * projList.add(Projections.property("reviewTypeDescription"),
		 * "reviewTypeDescription"); projList.add(Projections.property("description"),
		 * "description");
		 * searchCriteria.setProjection(projList).setResultTransformer(Transformers.
		 * aliasToBean(Committee.class)); countCriteria.add(and);
		 * 
		 * Long dashboardCount = (Long)
		 * countCriteria.setProjection(Projections.rowCount()).uniqueResult();
		 * logger.info("dashboardCount : " + dashboardCount);
		 * dashBoardProfile.setTotalServiceRequest(dashboardCount.intValue());
		 * 
		 * int count = pageNumber * (currentPage - 1);
		 * searchCriteria.setFirstResult(count);
		 * searchCriteria.setMaxResults(pageNumber);
		 * 
		 * @SuppressWarnings("unchecked") List<Committee> committees =
		 * searchCriteria.list(); dashBoardProfile.setCommittees(committees); } catch
		 * (Exception e) { logger.error("Error in method getDashBoardDataForCommittee");
		 * e.printStackTrace(); }
		 */
		return dashBoardProfile;
	}

	@Override
	public DashBoardProfile getDashBoardDataForCommitteeSchedule(CommonVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		/*
		 * Integer pageNumber = vo.getPageNumber(); String sortBy = vo.getSortBy();
		 * String reverse = vo.getReverse(); String property1 = vo.getProperty1();
		 * String property2 = vo.getProperty2(); String property3 = vo.getProperty3();
		 * String property4 = vo.getProperty4(); Integer currentPage =
		 * vo.getCurrentPage(); Date filterStartDate = vo.getFilterStartDate(); Date
		 * filterEndDate = vo.getFilterEndDate();
		 * 
		 * Conjunction and = Restrictions.conjunction(); try {
		 * logger.info("----------- getDashBoardDataForCommitteeSchedule ------------");
		 * Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		 * Criteria searchCriteria = session.createCriteria(CommitteeSchedule.class);
		 * searchCriteria.createAlias("committee", "committee",
		 * JoinType.LEFT_OUTER_JOIN); Criteria countCriteria =
		 * session.createCriteria(CommitteeSchedule.class);
		 * countCriteria.createAlias("committee", "committee",
		 * JoinType.LEFT_OUTER_JOIN); if (sortBy.isEmpty() || reverse.isEmpty()) {
		 * searchCriteria.addOrder(Order.desc("updateTimestamp")); } else { if
		 * (reverse.equals("DESC")) { searchCriteria.addOrder(Order.desc(sortBy)); }
		 * else { searchCriteria.addOrder(Order.asc(sortBy)); } } if (property1 != null
		 * && !property1.isEmpty()) { and.add(Restrictions.like("scheduleId", "%" +
		 * property1 + "%").ignoreCase()); } if (property2 != null &&
		 * !property2.isEmpty()) { and.add(Restrictions.like("place", "%" + property2 +
		 * "%").ignoreCase()); } if (property3 != null && !property3.isEmpty()) {
		 * and.add(Restrictions.like("committee.committeeId", "%" + property3 +
		 * "%").ignoreCase()); } if (property4 != null && !property4.isEmpty()) {
		 * and.add(Restrictions.like("committee.committeeName", "%" + property4 +
		 * "%").ignoreCase()); }
		 * 
		 * searchCriteria.add(and); countCriteria.add(and);
		 * 
		 * Long dashboardCount = (Long)
		 * countCriteria.setProjection(Projections.rowCount()).uniqueResult();
		 * logger.info("dashboardCount : " + dashboardCount);
		 * dashBoardProfile.setTotalServiceRequest(dashboardCount.intValue());
		 * 
		 * int count = pageNumber * (currentPage - 1);
		 * searchCriteria.setFirstResult(count);
		 * searchCriteria.setMaxResults(pageNumber);
		 * 
		 * ProjectionList projList = Projections.projectionList();
		 * projList.add(Projections.property("scheduleId"), "scheduleId");
		 * projList.add(Projections.property("scheduledDate"), "scheduledDate");
		 * projList.add(Projections.property("place"), "place");
		 * projList.add(Projections.property("protocolSubDeadline"),
		 * "protocolSubDeadline"); projList.add(Projections.property("committee"),
		 * "committee"); projList.add(Projections.property("scheduleStatus"),
		 * "scheduleStatus");
		 * 
		 * searchCriteria.setProjection(projList) .setResultTransformer(new
		 * AliasToBeanResultTransformer(CommitteeSchedule.class));
		 * 
		 * @SuppressWarnings("unchecked") List<CommitteeSchedule> committeeSchedules =
		 * searchCriteria.list(); Date scheduleDate = null; if (filterStartDate != null
		 * && filterEndDate != null) { Date startDate =
		 * DateUtils.addDays(filterStartDate, -1); Date endDate =
		 * DateUtils.addDays(filterEndDate, 1); List<CommitteeSchedule>
		 * filteredSchedules = new ArrayList<CommitteeSchedule>(); for
		 * (CommitteeSchedule schedule : committeeSchedules) { scheduleDate =
		 * schedule.getScheduledDate(); if ((scheduleDate != null) &&
		 * scheduleDate.after(startDate) && scheduleDate.before(endDate)) {
		 * filteredSchedules.add(schedule); } }
		 * dashBoardProfile.setCommitteeSchedules(filteredSchedules); } else {
		 * dashBoardProfile.setCommitteeSchedules(committeeSchedules); } } catch
		 * (Exception e) {
		 * logger.error("Error in method getDashBoardDataForCommitteeSchedule");
		 * e.printStackTrace(); }
		 */
		return dashBoardProfile;
	}

	@Override
	public DashBoardProfile getDashBoardDataForGrantCall(GrantCallDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<GrantCall> grantCalls = new ArrayList<>();
		String isAdvancedSearch = vo.getAdvancedSearch();
		String personId = vo.getPersonId();
		Integer pageNumber = vo.getPageNumber();
		String grantCallId = vo.getProperty1();
		String grantCallTitle = vo.getProperty2();
		List<Integer> grantCallType = vo.getProperty3();
		String fundingAgency = vo.getProperty4();
		List<Integer> grantCallStatus = vo.getProperty5();
		List<String> relevantFields = vo.getProperty6();
		Boolean isCalenderRequired = vo.getIsCalenderRequired();
		String fundingScheme = vo.getProperty7();
		Integer currentPage = vo.getCurrentPage();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_GRANTCALL_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, grantCallId);
				statement.setString(2, grantCallTitle);
				List<String> grantCallTypes = new ArrayList<>();
				for(Integer type : grantCallType) {
					grantCallTypes.add(type.toString());
				}
				statement.setString(3, String.join(",", grantCallTypes));
				statement.setString(4, fundingAgency);
				List<String> grantCallStatuses = new ArrayList<>();
				for(Integer status : grantCallStatus) {
					grantCallStatuses.add(status.toString());
				}
				statement.setString(5, String.join(",", grantCallStatuses));
				statement.setString(6, String.join(",", relevantFields));
				statement.setString(7, fundingScheme);
				statement.setString(8, personId);
				statement.setString(9, setGrantCallOrder(sort));
				statement.setInt(10, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(11, (pageNumber == null ? 0 : pageNumber));
				statement.setString(12, isAdvancedSearch);
				statement.setBoolean(13, isDownload);
				statement.setBoolean(14, isCalenderRequired);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call GET_GRANTCALL_DASHBOARD (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?}");
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, grantCallId);
				statement.setString(3, grantCallTitle);
				List<String> grantCallTypes = new ArrayList<>();
				for(Integer type : grantCallType) {
					grantCallTypes.add(type.toString());
				}
				statement.setString(4, String.join(",", grantCallTypes));
				statement.setString(5, fundingAgency);
				List<String> grantCallStatuses = new ArrayList<>();
				for(Integer status : grantCallStatus) {
					grantCallStatuses.add(status.toString());
				}
				statement.setString(6, String.join(",", grantCallStatuses));
				statement.setString(7, String.join(",", relevantFields));
				statement.setString(8, fundingScheme);
				statement.setString(9, personId);
				statement.setString(10, setGrantCallOrder(sort));
				statement.setInt(11, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(12,(pageNumber == null ? 0 : pageNumber));
				statement.setString(13, isAdvancedSearch);
				statement.setBoolean(14, isDownload);
				statement.setBoolean(15, isCalenderRequired);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				GrantCall grantCall = new GrantCall();
				grantCall.setGrantCallId(resultSet.getInt("GRANT_HEADER_ID"));
				if (resultSet.getString("NAME") != null) {
					grantCall.setGrantCallName(resultSet.getString("NAME"));
				}
				if (resultSet.getString("GRANT_CALL_TYPE") != null) {
					grantCall.setGrantCallTypeDesc(resultSet.getString("GRANT_CALL_TYPE"));
				}
				if (resultSet.getString("SPONSOR_CODE") != null) {
					grantCall.setSponsorCode(resultSet.getString("SPONSOR_CODE"));
				}
				if (resultSet.getString("SPONSOR_NAME") != null) {
					grantCall.setSponsorName(commonService.getSponsorFormatBySponsorDetail(resultSet.getString("SPONSOR_CODE"), resultSet.getString("SPONSOR_NAME"), resultSet.getString("ACRONYM")));
				}
				if (resultSet.getString("GRANT_STATUS_CODE") != null) {
					grantCall.setGrantStatusCode(resultSet.getInt("GRANT_STATUS_CODE" ));
				}
				if (resultSet.getString("GRANT_CALL_STATUS") != null) {
					grantCall.setGrantCallStatusDesc(resultSet.getString("GRANT_CALL_STATUS"));
				}
				if (resultSet.getDate("OPENING_DATE") != null) {
					grantCall.setOpeningDate(resultSet.getTimestamp("OPENING_DATE"));
				}
				if (resultSet.getDate("CLOSING_DATE") != null) {
					grantCall.setClosingDate(resultSet.getTimestamp("CLOSING_DATE"));
				}
				if (resultSet.getDate("INTERNAL_SUBMISSION_DEADLINE_DATE") != null) {
					grantCall.setInternalSubmissionDeadLineDate(resultSet.getTimestamp("INTERNAL_SUBMISSION_DEADLINE_DATE"));
				}
				if (resultSet.getString("RELEVANT_FIELD_DESCRIPTION") != null) {
					grantCall.setRelevantFields( Arrays.asList(resultSet.getString("RELEVANT_FIELD_DESCRIPTION").split(" , ")));
				}
				grantCalls.add(grantCall);
			}
			dashBoardProfile.setGrantCalls(grantCalls);
			dashBoardProfile.setTotalServiceRequest(getDashboardGrantCallCount(vo));
		} catch (SQLException e) {
			logger.info("Exception in dashboardGrantCalls : ", e);
			e.printStackTrace();
		}
		return dashBoardProfile;
	}

	private Integer getDashboardGrantCallCount(GrantCallDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer count = 0;
		String isAdvancedSearch = vo.getAdvancedSearch();
		String personId = vo.getPersonId();
		String grantCallId = vo.getProperty1();
		String grantCallTitle = vo.getProperty2();
		List<Integer> grantCallType = vo.getProperty3();
		String fundingAgency = vo.getProperty4();
		List<Integer> grantCallStatus = vo.getProperty5();
		List<String> relevantFields = vo.getProperty6();
		String fundingScheme = vo.getProperty7();
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_GRANTCALL_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, grantCallId);
				statement.setString(2, grantCallTitle);
				List<String> grantCallTypes = new ArrayList<>();
				for(Integer type : grantCallType) {
					grantCallTypes.add(type.toString());
				}
				statement.setString(3, String.join(",", grantCallTypes));
				statement.setString(4, fundingAgency);
				List<String> grantCallStatuses = new ArrayList<>();
				for(Integer status : grantCallStatus) {
					grantCallStatuses.add(status.toString());
				}
				statement.setString(5, String.join(",", grantCallStatuses));
				statement.setString(6, String.join(",", relevantFields));
				statement.setString(7, fundingScheme);
				statement.setString(8, personId);
				statement.setString(9, setGrantCallOrder(sort));
				statement.setInt(10, 0);
				statement.setInt(11, 0);
				statement.setString(12, isAdvancedSearch);
				statement.setBoolean(13, true);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call GET_GRANTCALL_DASHBOARD_COUNT (?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, grantCallId);
				statement.setString(3, grantCallTitle);
				List<String> grantCallTypes = new ArrayList<>();
				for(Integer type : grantCallType) {
					grantCallTypes.add(type.toString());
				}
				statement.setString(4, String.join(",", grantCallTypes));
				statement.setString(5, fundingAgency);
				List<String> grantCallStatuses = new ArrayList<>();
				for(Integer status : grantCallStatus) {
					grantCallStatuses.add(status.toString());
				}
				statement.setString(6, String.join(",", grantCallStatuses));
				statement.setString(7, String.join(",", relevantFields));
				statement.setString(8, fundingScheme);
				statement.setString(9, personId);
				statement.setString(10, setGrantCallOrder(sort));
				statement.setInt(11, 0);
				statement.setInt(12, 0);
				statement.setString(13, isAdvancedSearch);
				statement.setBoolean(14, true);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.info("Exception in grantCallDashboardCount {}", e.getMessage());
			e.printStackTrace();
		}
		return count;
	}

	private String setGrantCallOrder(Map<String, String> sort) {
		String sortOrder = null;
		if (!sort.isEmpty()) {
			for (Map.Entry<String, String> mapElement : sort.entrySet()) {
				if (mapElement.getKey().equals("grantCallId")) {
					sortOrder = (sortOrder == null ? "T.GRANT_HEADER_ID " + mapElement.getValue() : sortOrder + ", T.GRANT_HEADER_ID " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("grantCallName")) {
					sortOrder = (sortOrder == null ? "T.NAME " + mapElement.getValue() : sortOrder + ", T.NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("grantCallStatus.description")) {
					sortOrder = (sortOrder == null ? "T.GRANT_CALL_STATUS " + mapElement.getValue() : sortOrder + ", T.GRANT_CALL_STATUS " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("grantCallType.description")) {
					sortOrder = (sortOrder == null ? "T.GRANT_CALL_TYPE " + mapElement.getValue() : sortOrder + ", T.GRANT_CALL_TYPE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("sponsor.sponsorName")) {
					sortOrder = (sortOrder == null ? "T.SPONSOR_NAME " + mapElement.getValue() : sortOrder + ", T.SPONSOR_NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("openingDate")) {
					sortOrder = (sortOrder == null ? "T.OPENING_DATE " + mapElement.getValue() : sortOrder + ", T.OPENING_DATE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("closingDate")) {
					sortOrder = (sortOrder == null ? "T.CLOSING_DATE " + mapElement.getValue() : sortOrder + ", T.CLOSING_DATE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("internalSubmissionDeadLineDate")) {
					sortOrder = (sortOrder == null ? "T.INTERNAL_SUBMISSION_DEADLINE_DATE " + mapElement.getValue() : sortOrder + ", T.INTERNAL_SUBMISSION_DEADLINE_DATE " + mapElement.getValue());
				}
			}
		}
		return sortOrder;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List<Object[]> getDashBoardDataForAwardForDownload(String personId, String sponsorCode,
			List<Object[]> awards) throws Exception {
		try {
			logger.info("----------- getDashBoardDataForAwardForDownload ------------");
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query proposalList = session.createSQLQuery(
					"SELECT T1.PROPOSAL_ID, T1.TITLE, T2.SPONSOR_NAME, T4.DESCRIPTION AS PROPOSAL_TYPE, T3.FULL_NAME AS PI, T1.SUBMISSION_DATE FROM EPS_PROPOSAL T1 INNER JOIN SPONSOR T2 ON T1.SPONSOR_CODE = T2.SPONSOR_CODE LEFT OUTER JOIN EPS_PROPOSAL_PERSONS T3 ON T1.PROPOSAL_ID = T3.PROPOSAL_ID AND T3.PROP_PERSON_ROLE_ID = 3 INNER JOIN EPS_PROPOSAL_TYPE T4 ON T1.TYPE_CODE=T4.TYPE_CODE WHERE T2.SPONSOR_TYPE_CODE = :sponsorCode AND T1.HOME_UNIT_NUMBER IN(SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME = 'VIEW_PROPOSAL' AND person_id = :personId)");
			proposalList.setString("personId", personId).setString("sponsorCode", sponsorCode);
			awards = proposalList.list();
			logger.info("dashBoardDataForAward : " + awards);
		} catch (Exception e) {
			logger.error("Error in method getDashBoardDataForAwardForDownload");
			e.printStackTrace();
		}
		return awards;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List<Object[]> getProtocolDashboardDataForDownload(String personId, String sponsorCode,
			List<Object[]> protocols) throws Exception {
		try {
			logger.info("----------- getProtocolDashboardDataForDownload ------------");
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query proposalList = session.createSQLQuery(
					"SELECT T1.PROPOSAL_ID, T1.TITLE, T2.SPONSOR_NAME, T4.DESCRIPTION AS PROPOSAL_TYPE, T3.FULL_NAME AS PI, T1.SUBMISSION_DATE FROM EPS_PROPOSAL T1 INNER JOIN SPONSOR T2 ON T1.SPONSOR_CODE = T2.SPONSOR_CODE LEFT OUTER JOIN EPS_PROPOSAL_PERSONS T3 ON T1.PROPOSAL_ID = T3.PROPOSAL_ID AND T3.PROP_PERSON_ROLE_ID = 3 INNER JOIN EPS_PROPOSAL_TYPE T4 ON T1.TYPE_CODE=T4.TYPE_CODE WHERE T2.SPONSOR_TYPE_CODE = :sponsorCode AND T1.HOME_UNIT_NUMBER IN(SELECT DISTINCT UNIT_NUMBER FROM PERSON_ROLE_RT WHERE RIGHT_NAME = 'VIEW_PROPOSAL' AND person_id = :personId)");
			proposalList.setString("personId", personId).setString("sponsorCode", sponsorCode);
			protocols = proposalList.list();
			logger.info("dashBoardDataForProtocols : " + protocols);
		} catch (Exception e) {
			logger.error("Error in method getProtocolDashboardDataForDownload");
			e.printStackTrace();
		}
		return protocols;
	}

	@Override
	public DashBoardProfile getDashBoardDataForNegotiations(NegotiationDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<Negotiations> negotiations = new ArrayList<Negotiations>();
		String propertyNegotiationId = vo.getProperty1();
		String propertyNegotiatorFullName = vo.getProperty2();
		List<String> propertyStatus = vo.getProperty3();
		List<String> propertyAgreementType = vo.getProperty4();
		String propertyTitle = vo.getProperty5();
		String propertySponsor = vo.getProperty6();
		String personId = vo.getPersonId();
		String tabType = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		String sortBy = vo.getSortBy();
		String reverse = vo.getReverse();
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_NEGOTIATION_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, tabType);
				statement.setString(2, personId);
				statement.setBigDecimal(3, propertyNegotiationId.isEmpty() ? null : new BigDecimal(propertyNegotiationId));
				statement.setString(4, propertyNegotiatorFullName);
				statement.setString(5, String.join(",", propertyAgreementType));
				statement.setString(6, String.join(",", propertyStatus));
				statement.setString(7, propertySponsor);
				statement.setString(8, propertyTitle);
				statement.setString(9, sortNegotiation(sortBy,reverse));
				statement.setInt(10, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(11, (pageNumber == null ? 0 : pageNumber));
				statement.setString(12, isAdvancedSearch);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_NEGOTIATION_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, tabType);
				statement.setString(3, personId);
				statement.setBigDecimal(4, propertyNegotiationId.isEmpty() ? null : new BigDecimal(propertyNegotiationId));
				statement.setString(5, propertyNegotiatorFullName);
				statement.setString(6, String.join(",", propertyAgreementType));
				statement.setString(7, String.join(",", propertyStatus));
				statement.setString(8, propertySponsor);
				statement.setString(9, propertyTitle);
				statement.setString(10, sortNegotiation(sortBy,reverse));
				statement.setInt(11, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(12,(pageNumber == null ? 0 : pageNumber));
				statement.setString(13, isAdvancedSearch);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				Negotiations negotiation = new Negotiations();
				if (resultSet.getString("TITLE") != null) {
					negotiation.setTitle((resultSet.getString("TITLE")));
				}
				negotiation.setNegotiationId((resultSet.getInt("NEGOTIATION_ID")));
				if (resultSet.getString("NEGOTIATOR_FULL_NAME") != null) {
					negotiation.setNegotiatorFullName((resultSet.getString("NEGOTIATOR_FULL_NAME")));
				}
				if (resultSet.getDate("START_DATE") != null) {
					negotiation.setStartDate((resultSet.getTimestamp("START_DATE")));
				}
				if (resultSet.getDate("UPDATE_TIMESTAMP") != null) {
					negotiation.setUpdateTimeStamp((resultSet.getTimestamp("UPDATE_TIMESTAMP")));
				}
				if (resultSet.getString("AGREEMENT_TYPE") != null) {
					negotiation.setAgreementTypeCode((resultSet.getString("AGREEMENT_TYPE")));
				}
				if (resultSet.getString("SPONSOR_NAME") != null) {
					negotiation.setSponsor((resultSet.getString("SPONSOR_NAME")));
				}
				if (resultSet.getString("NEGOTIATION_STATUS") != null) {
					negotiation.setNegotiationStatus((resultSet.getString("NEGOTIATION_STATUS")));
				}
				if (resultSet.getString("NEGOTIATION_STATUS_CODE") != null) {
					negotiation.setNegotiationStatusCode(resultSet.getString("NEGOTIATION_STATUS_CODE"));
				}
				if (resultSet.getString("UPDATE_USER") != null) {
					negotiation.setUpdateUserFullName(personDao.getUserFullNameByUserName(resultSet.getString("UPDATE_USER")));
				}
				negotiations.add(negotiation);
			}
			dashBoardProfile.setNegotiationsList(negotiations);
			dashBoardProfile.setTotalServiceRequest(getDashboardNegotiationCount(propertyNegotiationId,propertyNegotiatorFullName,propertyStatus,propertyAgreementType,propertyTitle,propertySponsor,personId,tabType,currentPage,pageNumber,isAdvancedSearch,sort));
		} catch (SQLException e) {
			logger.info("exception in dashboardNegotiations : " + e);
			e.printStackTrace();
		}
		return dashBoardProfile;	
	}
	
	private String sortNegotiation(String sortBy, String reverse) {
		String sort = null;
		if(sortBy.equals("negotiatorFullName")) {
			sort = "T.NEGOTIATOR_FULL_NAME" + " "+ reverse;
		}else if(sortBy.equals("negotiationsStatus.description")) {
			sort = "T.NEGOTIATION_STATUS" + " "+ reverse;
		}else if(sortBy.equals("startDate")) {
			sort = "T.START_DATE" + " "+ reverse;
		}else if(sortBy.equals("negotiationsAgreementType.description")) {
			sort = "T.AGREEMENT_TYPE" + " "+ reverse;
		}else if(sortBy.equals("updateTimeStamp")) {
			sort = "T.UPDATE_TIMESTAMP" + " "+ reverse;
		}else if(sortBy.equals("sponsor")) {
			sort = "T.SPONSOR_NAME" + " "+ reverse;
		}else if(sortBy.equals("negotiationId")) {
			sort = "T.NEGOTIATION_ID" + " "+ reverse;
		}else if(sortBy.equals("title")) {
			sort = "T.TITLE" + " "+ reverse;
		}
		return sort;
	}

	private Integer getDashboardNegotiationCount(String propertyNegotiationId,String propertyNegotiatorFullName,List<String> propertyStatus,List<String> propertyAgreementType,String propertyTitle,String propertySponsor,String personId,String tabType,Integer currentPage,Integer pageNumber,String isAdvancedSearch,Map<String, String> sort) {
		Integer count = 0;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_NEGOTIATION_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, tabType);
				statement.setString(2, personId);
				statement.setBigDecimal(3, propertyNegotiationId.isEmpty() ? null : new BigDecimal(propertyNegotiationId));
				statement.setString(4, propertyNegotiatorFullName);
				statement.setString(5, String.join(",", propertyAgreementType));
				statement.setString(6, String.join(",", propertyStatus));
				statement.setString(7, propertySponsor);
				statement.setString(8, propertyTitle);
				statement.setString(9, setOrder(sort));
				statement.setInt(10, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(11, (pageNumber == null ? 0 : pageNumber));
				statement.setString(12, isAdvancedSearch);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureNameForCount = "GET_NEGOTIATION_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureNameForCount + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, tabType);
				statement.setString(3, personId);
				statement.setBigDecimal(4, propertyNegotiationId.isEmpty() ? null : new BigDecimal(propertyNegotiationId));
				statement.setString(5, propertyNegotiatorFullName);
				statement.setString(6, String.join(",", propertyAgreementType));
				statement.setString(7, String.join(",", propertyStatus));
				statement.setString(8, propertySponsor);
				statement.setString(9, propertyTitle);
				statement.setString(10, setOrder(sort));
				statement.setInt(11, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(12,(pageNumber == null ? 0 : pageNumber));
				statement.setString(13, isAdvancedSearch);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.info("exception in dashboardNegotiations : " + e);
			e.printStackTrace();
		}
		return count;
	}

	@Override
	public DashBoardProfile getDashBoardDataForInstProposal(InstituteProposalDashboardVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<InstituteProposal> instProposals = new ArrayList<>();
		ResultSet resultSet = getResultSetForInstituteProposal(vo);
		try {
			if (resultSet != null) {
				while (resultSet.next()) {
					InstituteProposal instituteProposal = new InstituteProposal();
					instituteProposal.setProposalId(resultSet.getInt("PROPOSAL_ID"));
					instituteProposal.setProposalNumber(resultSet.getString("PROPOSAL_NUMBER"));
					instituteProposal.setTitle(resultSet.getString("TITLE"));
					instituteProposal.setPrincipalInvestigator(resultSet.getString("FULL_NAME"));
					instituteProposal.setApplicationActivityType(resultSet.getString("ACTIVITY_TYPE"));
					instituteProposal.setApplicationType(resultSet.getString("PROPOSAL_TYPE"));
					instituteProposal.setApplicationStatus(resultSet.getString("PROPOSAL_STATUS"));
					instituteProposal.setSponsorCode(resultSet.getString("SPONSOR_CODE"));
					instituteProposal.setSponsorName(commonService.getSponsorFormatBySponsorDetail(resultSet.getString("SPONSOR_CODE"), resultSet.getString("SPONSOR_NAME"), resultSet.getString("ACRONYM")));
					instituteProposal.setSubmissionDate(resultSet.getTimestamp("SUBMISSION_DATE"));
					instProposals.add(instituteProposal);
				}
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		dashBoardProfile.setInstituteProposal(instProposals);
		dashBoardProfile.setTotalServiceRequest(getDashBoardDataCountForInstProposal(vo));
		return dashBoardProfile;
	}

	private Integer getDashBoardDataCountForInstProposal(InstituteProposalDashboardVO vo) {
		vo.setIsCount(true);
		ResultSet resultSet = getResultSetForInstituteProposal(vo);
		if (resultSet != null) {
			try {
				while(resultSet.next()) {
					return Integer.parseInt(resultSet.getString(1));
				}
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
		return null;
	}

	private ResultSet getResultSetForInstituteProposal(InstituteProposalDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		List<Integer> property3 = vo.getProperty3();
		List<String> property4 = vo.getProperty4();
		String property5 = vo.getProperty5();
		String property6 = vo.getProperty6();
		List<Integer> property7 = vo.getProperty7();
		String personId = vo.getPersonId();	
		String tabName = vo.getTabName();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Integer startIndex  = 0;
		Integer itemsPerPage = 0;
		Boolean isDownload = true;
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				if (vo.getIsCount().equals(Boolean.FALSE)) {
					statement = connection.prepareCall("{call GET_INST_PROPOSAL_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
					startIndex = vo.getCurrentPage() == null ? 0 : vo.getCurrentPage() -1;
					itemsPerPage = vo.getPageNumber() == null ? 0: vo.getPageNumber();
					isDownload = vo.getIsDownload();
				} else {
					statement = connection.prepareCall("{call GET_INST_PROPOSAL_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				}
				statement.setString(1, property1);
				statement.setString(2, property2);
				List<String> statuses = new ArrayList<>();
				for(Integer status : property3) {
					statuses.add(status.toString());
				}
				statement.setString(3, String.join(",", statuses));
				statement.setString(4, String.join(",", property4));
				statement.setString(5, property5);
				statement.setString(6, property6);
				statement.setString(7, personId);
				statement.setString(8, setInstituteProposalSortOrder(sort));
				statement.setInt(9, startIndex);
				statement.setInt(10, itemsPerPage);
				statement.setString(11, tabName);
				statement.setBoolean(12, isDownload);
				statement.setString(13, isAdvancedSearch);
				List<String> proposalTypes = new ArrayList<>();
				for(Integer proposalType : property7) {
					proposalTypes.add(proposalType.toString());
				}
				statement.setString(14, String.join(",", proposalTypes));
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				if (vo.getIsCount().equals(Boolean.FALSE)) {
					statement = connection.prepareCall("{call GET_INST_PROPOSAL_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
					startIndex = vo.getCurrentPage()-1;
					itemsPerPage = vo.getPageNumber();
					isDownload = vo.getIsDownload();
				} else {
					statement = connection.prepareCall("{call GET_INST_PROPOSAL_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				}
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, property1);
				statement.setString(3, property2);
				List<String> statuses = new ArrayList<>();
				for(Integer status : vo.getProperty3()) {
					statuses.add(status.toString());
				}
				statement.setString(4, String.join(",", statuses));
				statement.setString(5, String.join(",", property4));
				statement.setString(6, property5);
				statement.setString(7, null);
				statement.setString(8, personId);
				statement.setString(9, setInstituteProposalSortOrder(sort));
				statement.setInt(10, startIndex);
				statement.setInt(11, itemsPerPage);
				statement.setString(12, tabName);
				statement.setBoolean(13, isDownload);
				statement.setString(14, isAdvancedSearch);
				List<String> proposalTypes = new ArrayList<>();
				for(Integer proposalType : property7) {
					proposalTypes.add(proposalType.toString());
				}
				statement.setString(15, String.join(",", proposalTypes));
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
		} catch (SQLException e) {
			logger.info("exception in dashboardAwards : " + e);
			e.printStackTrace();
		}
		return resultSet;
	}

	private String setInstituteProposalSortOrder(Map<String, String> sort) {
		String sortOrder = null;
		if (!sort.isEmpty()) {
			for (Map.Entry<String, String> mapElement : sort.entrySet()) {
				if (mapElement.getKey().equals("proposalNumber")) {
					sortOrder = (sortOrder == null ? "T.PROPOSAL_NUMBER " + mapElement.getValue() : sortOrder + ", T.PROPOSAL_NUMBER " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("title")) {
					sortOrder = (sortOrder == null ? "T.TITLE " + mapElement.getValue() : sortOrder + ", T.TITLE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("instProposalPersons.fullName")) {
					sortOrder = (sortOrder == null ? "T.FULL_NAME " + mapElement.getValue() : sortOrder + ", T.FULL_NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("activityType")) {
					sortOrder = (sortOrder == null ? "T.ACTIVITY_TYPE " + mapElement.getValue() : sortOrder + ", T.ACTIVITY_TYPE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("instProposalType.description")) {
					sortOrder = (sortOrder == null ? "T.PROPOSAL_TYPE " + mapElement.getValue() : sortOrder + ", T.PROPOSAL_TYPE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("instProposalStatus.description")) {
					sortOrder = (sortOrder == null ? "T.PROPOSAL_STATUS " + mapElement.getValue() : sortOrder + ", T.PROPOSAL_STATUS " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("sponsorName")) {
					sortOrder = (sortOrder == null ? "T.SPONSOR_NAME " + mapElement.getValue() : sortOrder + ", T.SPONSOR_NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("submissionDate")) {
					sortOrder = (sortOrder == null ? "T.SUBMISSION_DATE " + mapElement.getValue() : sortOrder + ", T.SUBMISSION_DATE " + mapElement.getValue());
				}
			}
		}
		return sortOrder;
	}

	/*@Override
	public DashBoardProfile getDashBoardDataForAgreement(AgreementDashboardVO vo) {
		logger.info("----------- get DashBoard Data For Agreement DAO ------------");
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		Integer pageNumber = vo.getPageNumber();
		String sortBy = vo.getSortBy();
		String reverse = vo.getReverse();
		String propertyAgreementId = vo.getProperty1();
		List<String> propertyAgreementTypes = vo.getProperty2();
		List<String> propertyStatusList = vo.getProperty3();
		String propertyProjectName = vo.getProperty4();
		Integer currentPage = vo.getCurrentPage();
		Conjunction and = Restrictions.conjunction();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Criteria searchCriteria = session.createCriteria(AgreementHeader.class);
			Criteria countCriteria = session.createCriteria(AgreementHeader.class);
			searchCriteria.createAlias("agreementStatus", "agreementStatus");
			countCriteria.createAlias("agreementStatus", "agreementStatus");
			searchCriteria.createAlias("agreementType", "agreementType");
			countCriteria.createAlias("agreementType", "agreementType");
			if (sortBy.isEmpty() || reverse.isEmpty()) {
				searchCriteria.addOrder(Order.desc("updateTimeStamp"));
			} else {
				if (reverse.equals("DESC")) {
					searchCriteria.addOrder(Order.desc(sortBy));
				} else {
					searchCriteria.addOrder(Order.asc(sortBy));
				}
			}
			if (propertyAgreementId != null && !propertyAgreementId.isEmpty()) {
				Integer agreementRequestId = Integer.valueOf(propertyAgreementId);
				and.add(Restrictions.like("agreementRequestId", agreementRequestId));
			}
			if (propertyProjectName != null && !propertyProjectName.isEmpty()) {
				and.add(Restrictions.like("title", "%" + propertyProjectName + "%").ignoreCase());
			}
			if (propertyStatusList != null && !propertyStatusList.isEmpty()) {
				and.add(Restrictions.in("agreementStatus.agreementStatusCode", propertyStatusList));
			}
			if (propertyAgreementTypes != null && !propertyAgreementTypes.isEmpty()) {
				and.add(Restrictions.in("agreementType.agreementTypeCode", propertyAgreementTypes));
			}		 
			searchCriteria.add(and);
			searchCriteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
			countCriteria.add(and);
			countCriteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
			searchCriteria.setProjection(Projections.distinct(Projections.id()));
			countCriteria.setProjection(Projections.distinct(Projections.id()));
			Long dashboardCount = (Long) countCriteria.setProjection(Projections.distinct(Projections.countDistinct("agreementRequestId"))).uniqueResult();
			logger.info("dashboardCount : " + dashboardCount);
			dashBoardProfile.setTotalServiceRequest(dashboardCount.intValue());
			int count = pageNumber * (currentPage - 1);
			searchCriteria.setFirstResult(count);
			searchCriteria.setMaxResults(pageNumber);
			@SuppressWarnings("unchecked")
			List<Integer> agreementHeaderIds = searchCriteria.list();			
			List<AgreementHeader> agreementHeaderList = new ArrayList<>();
			if (agreementHeaderIds != null && !agreementHeaderIds.isEmpty()) {
				for (Integer agreementHeaderId : agreementHeaderIds) {
					AgreementHeader agreementHeader = agreementDao.getAgreementById(agreementHeaderId);
					AgreementHeader agreementObj = new AgreementHeader();
					agreementObj.setAgreementRequestId(agreementHeader.getAgreementRequestId());
					agreementObj.setAgreementType(agreementHeader.getAgreementType());
					agreementObj.setTitle(agreementHeader.getTitle());
					agreementObj.setAgreementStatus(agreementHeader.getAgreementStatus());
					agreementObj.setStartDate(agreementHeader.getStartDate());				
					agreementObj.setUpdateTimeStamp(agreementHeader.getUpdateTimeStamp());
					agreementObj.setUpdateUserFullName(personDao.getUserFullNameByUserName(agreementHeader.getUpdateUser()));
					agreementHeaderList.add(agreementObj);
				}
			}
			dashBoardProfile.setAgreementHeaderList(agreementHeaderList);
		} catch (Exception e) {
			logger.error("Error in method getDashBoardData For Agreement ", e.getMessage());
		}
		return dashBoardProfile;
	}*/

	@Override
	public DashBoardProfile getDashBoardDataForAgreement(AgreementDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<AgreementHeader> agreementHeaders = new ArrayList<>();
		String agreementRequestId = vo.getProperty1();
		String agreementTitle = vo.getProperty2();
		String requestorPersonId = vo.getProperty5();
		String leadUnit = vo.getProperty7();
		List<String> agreementCategory = vo.getProperty3();
		List<String> agreementType = vo.getProperty4();
		String personId = vo.getPersonId();
		List<String> agreementStatus = vo.getProperty6();
		String tabType = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		String sortBy = vo.getSortBy();
		String reverse = vo.getReverse();
		Boolean isDownload = vo.getIsDownload();
		String negoPersonId = vo.getProperty9();
		String piPersonId = vo.getProperty10();
		String catPersonId = vo.getProperty11();
		String stoPersonId = vo.getProperty12();
		String caPersonId = vo.getProperty13();
		List<String> reviewStatus = vo.getProperty14();
		List<String> sponsorTypes = vo.getProperty15();
		String organization = vo.getProperty8();
		String projectId = vo.getProperty19();
		List<String> adminGroupId = vo.getProperty20();
		try {
			if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
				statement = connection.prepareCall("{call GET_AGREEMENT_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, agreementRequestId);
				statement.setString(2, agreementTitle);
				statement.setString(3, requestorPersonId);
				statement.setString(4, String.join(",", agreementCategory));
				statement.setString(5, String.join(",", agreementType));
				statement.setString(6, String.join(",", agreementStatus));
				statement.setString(7, personId);
				statement.setString(8, sortAgreement(sortBy, reverse));
				statement.setInt(9, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(10, (pageNumber == null ? 0 : pageNumber));
				statement.setString(11, tabType);
				statement.setBoolean(12, isDownload);
				statement.setString(13, isAdvancedSearch);
				statement.setString(14, leadUnit);
				statement.setString(15, organization);
				statement.setString(16, negoPersonId);
				statement.setString(17, piPersonId);
				statement.setString(18, catPersonId);
				statement.setString(19, stoPersonId);
				statement.setString(20, caPersonId);
				statement.setString(21, String.join(",", reviewStatus));
				statement.setString(22, String.join(",", sponsorTypes));
				statement.setString(23, vo.getProperty16());
				statement.setString(24, vo.getProperty17());
				statement.setString(25, vo.getProperty18());
				statement.setString(26, projectId);
				statement.setString(27, String.join(",", adminGroupId));
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
				String procedureName = "GET_AGREEMENT_DASHBOARD";
				String functionCall = "{call " + procedureName
						+ "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setString(1, agreementRequestId);
				statement.setString(2, agreementTitle);
				statement.setString(3, requestorPersonId);
				statement.setString(4, String.join(",", agreementCategory));
				statement.setString(5, String.join(",", agreementType));
				statement.setString(6, String.join(",", agreementStatus));
				statement.setString(7, personId);
				statement.setString(8, sortAgreement(sortBy, reverse));
				statement.setInt(9, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(10, (pageNumber == null ? 0 : pageNumber));
				statement.setString(11, tabType);
				statement.setString(12, isDownload.toString().toUpperCase());
				statement.setString(13, isAdvancedSearch);
				statement.setString(14, leadUnit);
				statement.setString(15, organization);
				statement.setString(16, negoPersonId);
				statement.setString(17, piPersonId);
				statement.setString(18, catPersonId);
				statement.setString(19, stoPersonId);
				statement.setString(20, caPersonId);
				statement.setString(21, String.join(",", reviewStatus));
				statement.setString(22, String.join(",", sponsorTypes));
				statement.setString(23, vo.getProperty16());
				statement.setString(24, vo.getProperty17());
				statement.setString(25, vo.getProperty18());
				statement.setString(26, projectId);
				statement.setString(27, String.join(",", adminGroupId));
				statement.registerOutParameter(28, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(28);
			}
			while (resultSet != null && resultSet.next()) {
				AgreementHeader agreementHeader = new AgreementHeader();
				if (resultSet.getString("TITLE") != null) {
					agreementHeader.setTitle((resultSet.getString("TITLE")));
				}
				agreementHeader.setAgreementRequestId((resultSet.getInt("AGREEMENT_REQUEST_ID")));
				if (resultSet.getDate("START_DATE") != null) {
					agreementHeader.setStartDate((resultSet.getTimestamp("START_DATE")));
				}
				if (resultSet.getDate("UPDATE_TIMESTAMP") != null) {
					agreementHeader.setUpdateTimestamp((resultSet.getTimestamp("UPDATE_TIMESTAMP")));
				}
				if (resultSet.getString("TYPE") != null) {
					agreementHeader.setAgreementTypeCode((resultSet.getString("TYPE")));
				}
				if (resultSet.getDate("END_DATE") != null) {
					agreementHeader.setEndDate((resultSet.getTimestamp("END_DATE")));
				}
				if (resultSet.getString("CATEGORY_NAME") != null) {
					agreementHeader.setCategoryCode((resultSet.getString("CATEGORY_NAME")));
				}
				if (resultSet.getString("AGREEMENT_STATUS_CODE") != null) {
					agreementHeader.setAgreementStatusCode(resultSet.getString("AGREEMENT_STATUS_CODE"));
				}
				if (resultSet.getString("STATUS") != null) {
					agreementHeader.setStatusDescription(resultSet.getString("STATUS"));
				}
				if (resultSet.getString("UNIT") != null) {
					agreementHeader.setUnitName(resultSet.getString("UNIT"));
				}
				if (resultSet.getString("UNIT_NUMBER") != null) {
					agreementHeader.setUnitNumber(resultSet.getString("UNIT_NUMBER"));
				}
				if (resultSet.getString("CONTRACT_VALUE") != null) {
					BigDecimal contractValue = new BigDecimal(resultSet.getString("CONTRACT_VALUE"));
					agreementHeader.setContractValue(contractValue);
				}
				if (resultSet.getString("CURRENCY_CODE") != null) {
					agreementHeader.setCurrencyCode(resultSet.getString("CURRENCY_CODE"));
				}
				if (resultSet.getString("ORGANIZATION") != null) {
					agreementHeader.setOrganization((resultSet.getString("ORGANIZATION")));
				}
				if (resultSet.getString("NEGO_FULL_NAME") != null) {
					agreementHeader.setNegotiatorFullName(resultSet.getString("NEGO_FULL_NAME"));
				}
				if (resultSet.getString("PI_FULL_NAME") != null) {
					agreementHeader.setPiFullName(resultSet.getString("PI_FULL_NAME"));
				}
				if (resultSet.getString("CAT_FULL_NAME") != null) {
					agreementHeader.setCatFullName(resultSet.getString("CAT_FULL_NAME"));
				}
				if (resultSet.getString("STO_FULL_NAME") != null) {
					agreementHeader.setStoFullName(resultSet.getString("STO_FULL_NAME"));
				}
				if (resultSet.getString("CA_FULL_NAME") != null) {
					agreementHeader.setCaFullName((resultSet.getString("CA_FULL_NAME")));
				}
				if (resultSet.getString("REVIEW_STATUS") != null) {
					agreementHeader.setWorkflowStatusCode((resultSet.getString("REVIEW_STATUS")));
				}
				if (resultSet.getString("ADMIN_PERSON_NAME") != null) {
					agreementHeader.setAdminName((resultSet.getString("ADMIN_PERSON_NAME")));
				}
				if (resultSet.getString("NEGOTIATION_ID") != null) {
					agreementHeader.setNegotiationId((resultSet.getInt("NEGOTIATION_ID")));
				}
				if (resultSet.getString("REQUESTOR_NAME") != null) {
					agreementHeader.setRequestorName((resultSet.getString("REQUESTOR_NAME")));
				}
				agreementHeaders.add(agreementHeader);
			}
			dashBoardProfile.setAgreementHeaderList(agreementHeaders);
			if (Boolean.FALSE.equals(isDownload)) {
				dashBoardProfile.setInProgressCount(getDashboardAgreementCount(vo, "IN_PROGRESS_AGREEMENTS"));
				dashBoardProfile.setNewSubmissionCount(getDashboardAgreementCount(vo, "NEW_SUBMISSIONS"));
				dashBoardProfile.setAllAgreementCount(getDashboardAgreementCount(vo, "ALL_AGREEMENTS"));
				dashBoardProfile.setAllPendingAgreementCount(getDashboardAgreementCount(vo, "ALL_PENDING_AGREEMENTS"));
				dashBoardProfile.setMyPendingAgreementCount(getDashboardAgreementCount(vo, "MY_PENDING_AGREEMENTS"));
			}
		} catch (SQLException e) {
			logger.info("Exception in dashboard Agreement {}: ", e.getMessage());
		}
		return dashBoardProfile;	
	}

	private Integer getDashboardAgreementCount(AgreementDashboardVO vo, String tabType) {
		Integer count = 0;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		String agreementRequestId = vo.getProperty1();
		String agreementTitle = vo.getProperty2();
		String requestorPersonId = vo.getProperty5();
		String leadUnit = vo.getProperty7();
		List<String> agreementCategory = vo.getProperty3();
		List<String> agreementType = vo.getProperty4();
		String personId = vo.getPersonId();
		List<String> agreementStatus = vo.getProperty6();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		String sortBy = vo.getSortBy();
		String reverse = vo.getReverse();
		String negoPersonId = vo.getProperty9();
		String piPersonId = vo.getProperty10();
		String catPersonId = vo.getProperty11();
		String stoPersonId = vo.getProperty12();
		String caPersonId = vo.getProperty13();
		List<String> reviewStatus = vo.getProperty14();
		List<String> sponsorTypes = vo.getProperty15();
		String organization = vo.getProperty8();
		String projectId = vo.getProperty19();
		List<String> adminGroupId = vo.getProperty20();
		try {
			if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
				statement = connection.prepareCall("{call GET_AGREEMENT_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, agreementRequestId);
				statement.setString(2, agreementTitle);
				statement.setString(3, requestorPersonId);
				statement.setString(4, String.join(",", agreementCategory));
				statement.setString(5, String.join(",", agreementType));
				statement.setString(6, String.join(",", agreementStatus));
				statement.setString(7, personId);
				statement.setString(8, sortAgreement(sortBy,reverse));
				statement.setInt(9, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(10, (pageNumber == null ? 0 : pageNumber));
				statement.setString(11, tabType);
				statement.setBoolean(12, false);
				statement.setString(13, isAdvancedSearch);
				statement.setString(14, leadUnit);
				statement.setString(15, organization);
				statement.setString(16, negoPersonId);
				statement.setString(17, piPersonId);
				statement.setString(18, catPersonId);
				statement.setString(19, stoPersonId);
				statement.setString(20, caPersonId);
				statement.setString(21, String.join(",", reviewStatus));
				statement.setString(22, String.join(",", sponsorTypes));
				statement.setString(23, vo.getProperty16());
				statement.setString(24, vo.getProperty17());
				statement.setString(25, vo.getProperty18());
				statement.setString(26, projectId);
				statement.setString(27, String.join(",", adminGroupId));
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
				String procedureName = "GET_AGREEMENT_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setString(1, agreementRequestId);
				statement.setString(2, agreementTitle);
				statement.setString(3, requestorPersonId);
				statement.setString(4, String.join(",", agreementCategory));
				statement.setString(5, String.join(",", agreementType));
				statement.setString(6, String.join(",", agreementStatus));
				statement.setString(7, personId);
				statement.setString(8, sortAgreement(sortBy,reverse));
				statement.setInt(9, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(10, (pageNumber == null ? 0 : pageNumber));
				statement.setString(11, tabType);
				statement.setString(12, "FALSE");
				statement.setString(13, isAdvancedSearch);
				statement.setString(14, leadUnit);
				statement.setString(15, organization);
				statement.setString(16, negoPersonId);
				statement.setString(17, piPersonId);
				statement.setString(18, catPersonId);
				statement.setString(19, stoPersonId);
				statement.setString(20, caPersonId);
				statement.setString(21, String.join(",", reviewStatus));
				statement.setString(22, String.join(",", sponsorTypes));
				statement.setString(23, vo.getProperty16());
				statement.setString(24, vo.getProperty17());
				statement.setString(25, vo.getProperty18());
				statement.setString(26, projectId);
				statement.setString(27, String.join(",", adminGroupId));
				statement.registerOutParameter(28, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(28);
			}
			while (resultSet != null && resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.info("Exception in dashboard Agreement count : {}", e.getMessage());
		}
		return count;
	}

	private String sortAgreement(String sortBy, String reverse) {
		String sort = null;
		if (sortBy.equals("agreementTypeCode")) {
			sort = "T.TYPE" + " " + reverse;
		} else if (sortBy.equals("statusDescription")) {
			sort = "T.STATUS" + " " + reverse;
		} else if (sortBy.equals("startDate")) {
			sort = "T.START_DATE" + " " + reverse;
		} else if (sortBy.equals("endDate")) {
			sort = "T.END_DATE" + " " + reverse;
		} else if (sortBy.equals("agreementRequestId")) {
			sort = "T.AGREEMENT_REQUEST_ID" + " " + reverse;
		} else if (sortBy.equals("title")) {
			sort = "T.TITLE" + " " + reverse;
		} else if (sortBy.equals("categoryCode")) {
			sort = "T.CATEGORY_NAME" + " " + reverse;
		} else if (sortBy.equals("contractValue")) {
			sort = "T.CONTRACT_VALUE" + " " + reverse;
		} else if (sortBy.equals("unitNumber")) {
			sort = "T.UNIT" + " " + reverse;
		} else if (sortBy.equals("organization")) {
			sort = "T.ORGANIZATION" + " " + reverse;
		} else if (sortBy.equals("negotiatorFullName")) {
			sort = "T.NEGO_FULL_NAME" + " " + reverse;
		} else if (sortBy.equals("piFullName")) {
			sort = "T.PI_FULL_NAME" + " " + reverse;
		} else if (sortBy.equals("catFullName")) {
			sort = "T.CAT_FULL_NAME" + " " + reverse;
		} else if (sortBy.equals("stoFullName")) {
			sort = "T.STO_FULL_NAME" + " " + reverse;
		} else if (sortBy.equals("caFullName")) {
			sort = "T.CA_FULL_NAME" + " " + reverse;
		} else if (sortBy.equals("agreementAdminFullName")) {
			sort = "T.ADMIN_PERSON_NAME" + " " + reverse;
		} else if (sortBy.equals("requestorName")) {
			sort = "T.REQUESTOR_NAME" + " " + reverse;
		}
		return sort;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public List<Object[]> getDashBoardDataOfPersonForDownload(CommonVO vo, List<Object[]> personData) {
		try {
			logger.info("----------- getDashBoardDataOfPersonForDownload ------------");
			Query personList = null;
			String likeQuery = "";
			String mainQuery = "SELECT T1.PERSON_ID, T1.FIRST_NAME, T1.LAST_NAME, T1.MIDDLE_NAME, T1.USER_NAME, T1.EMAIL_ADDRESS, T1.PRIMARY_TITLE, T1.DIRECTORY_TITLE, T1.STATE, T1.HOME_UNIT, T3.UNIT_NAME FROM PERSON T1 INNER JOIN UNIT T3 ON T1.HOME_UNIT = T3.UNIT_NUMBER\n" + 
					"";
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			if (vo.getProperty1() != null && !vo.getProperty1().isEmpty()) {
				likeQuery = " and lower(T1.FIRST_NAME) like lower(:firstName) ";
			}
			if (vo.getProperty2() != null && !vo.getProperty2().isEmpty()) {
				likeQuery = likeQuery + " and lower(T1.LAST_NAME) like lower(:lastName) ";
			}
			personList = session.createSQLQuery(mainQuery + likeQuery);
			personData = personList.list();
			logger.info("persons : " + personData);
		} catch (Exception e) {
			logger.error("Error in method getDashBoardDataOfPersonForDownload");
			e.printStackTrace();
		}
		return personData;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public List<Object[]> getDataOfRolodexForDownload(CommonVO vo, List<Object[]> rolodexData) {
		try {
			logger.info("----------- getDataOfRolodexForDownload ------------");
			Query rolodexList = null;
			String likeQuery = "";
			String mainQuery = "SELECT T1.ROLODEX_ID, T1.LAST_NAME, T1.FIRST_NAME, T1.MIDDLE_NAME, T1.SPONSOR_CODE, T1.CITY, T1.ACTV_IND, T1.ORGANIZATION, T1.STATE, T1.COUNTY FROM ROLODEX T1 ;";
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			if (vo.getProperty1() != null && !vo.getProperty1().isEmpty()) {
				likeQuery = " and lower(T1.ROLODEX_ID) like lower(:rolodexId) ";
			}
			if (vo.getProperty2() != null && !vo.getProperty2().isEmpty()) {
				likeQuery = likeQuery + " and lower(T1.FULL_NAME) like lower(:fullName) ";
			}
			rolodexList = session.createSQLQuery(mainQuery + likeQuery);

			if (vo.getProperty1() != null && !vo.getProperty1().isEmpty()) {
				rolodexList.setString("rolodexId", "%" + vo.getProperty1() + "%");
			}
			if (vo.getProperty2() != null && !vo.getProperty2().isEmpty()) {
				rolodexList.setString("firstName", "%" + vo.getProperty2() + "%");
			}
			rolodexData = rolodexList.list();
			logger.info("persons : " + rolodexData);
		} catch (Exception e) {
			logger.error("Error in method getDataOfRolodexForDownload");
			e.printStackTrace();
		}
		return rolodexData;
	}

	@Override
	public DashBoardProfile dashboardDatasForProposal(ProposalDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<Proposal> proposals = new ArrayList<>();
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		List<String> propertyList3 = vo.getProperty3();
		String property3 = String.join(",", propertyList3);
		List<String> propertyList4 = vo.getProperty4();
		String property4 = String.join(",", propertyList4);
		String property5 = vo.getProperty5();
		List<String> propertyList6 = vo.getProperty6();
		String property6 = String.join(",", propertyList6);
		String property7 = vo.getProperty7();
		String property8 = vo.getProperty8();
		String property9 = vo.getProperty9();
		String property10 = vo.getProperty10();
		String property11 = vo.getProperty11();
		String property12 = vo.getProperty12();
		List<String> propertyList13 = vo.getProperty13();
		String property13 = String.join(",", propertyList13);
		String personId = vo.getPersonId();
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		String property14 = vo.getProperty14();
		String property15 = vo.getProperty15();
		String property16 = vo.getProperty16();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_PROPOSAL_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, property1);
				statement.setString(2, property2);
				statement.setString(3, property5);
				statement.setString(4, property4);
				statement.setString(5, property6);
				statement.setString(6, property7);
				statement.setString(7, property8);
				statement.setString(8, property3);
				statement.setString(9, property10);
				statement.setString(10, property9);
				statement.setString(11, property11);
				statement.setString(12, personId);
				statement.setString(13, setOrder(sort));
				statement.setInt(14, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(15, (pageNumber == null ? 0 : pageNumber));
				statement.setString(16, tabName);
				statement.setBoolean(17, isDownload);
				statement.setString(18, isAdvancedSearch);
				statement.setString(19, property12);
				statement.setString(20, property13);
				statement.setString(21, property14);
				statement.setString(22, property15);
				statement.setString(23, property16);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_PROPOSAL_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, property1);
				statement.setString(3, property2);
				statement.setString(4, property5);
				statement.setString(5, property4);
				statement.setString(6, property6);
				statement.setString(7, property7);
				statement.setString(8, property8);
				statement.setString(9, property3);
				statement.setString(10, property10);
				statement.setString(11, property9);
				statement.setString(12, property11);
				statement.setString(13, personId);
				statement.setString(14, setOrder(sort));
				statement.setInt(15, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(16, (pageNumber == null ? 0 : pageNumber));
				statement.setString(17, tabName);
				statement.setBoolean(18, isDownload);
				statement.setString(19, isAdvancedSearch);
				statement.setString(20, property12);
				statement.setString(21, property13);
				statement.setString(22, property14);
				statement.setString(23, property15);
				statement.setString(24, property16);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				Proposal proposal = new Proposal();
				proposal.setProposalId(resultSet.getInt("PROPOSAL_ID"));
				proposal.setTitle(resultSet.getString("TITLE"));
				ProposalPerson investigator = new ProposalPerson();
				investigator.setFullName(resultSet.getString("FULL_NAME"));
				investigator.setPersonId(resultSet.getString("PI_PERSON_ID"));
				proposal.setInvestigator(investigator);
				proposal.setHomeUnitName(resultSet.getString("HOME_UNIT_NAME"));
				proposal.setApplicationStatus(resultSet.getString("PROPOSAL_STATUS"));
				proposal.setSponsorCode(resultSet.getString("SPONSOR_CODE"));
				proposal.setSponsorName(commonService.getSponsorFormatBySponsorDetail(resultSet.getString("SPONSOR_CODE"), resultSet.getString("SPONSOR_NAME"), resultSet.getString("ACRONYM")));
				proposal.setGrantCallName(resultSet.getString("NAME"));
				proposal.setApplicationType(resultSet.getString("PROPOSAL_TYPE"));
				proposal.setGrantCallClosingDate(resultSet.getTimestamp("CLOSING_DATE"));
				proposal.setSponsorDeadlineDate(resultSet.getTimestamp("SPONSOR_DEADLINE_DATE"));
				proposal.setStatusCode(Integer.parseInt(resultSet.getString("STATUS_CODE")));
				proposal.setApplicationId(resultSet.getString("APPLICATION_ID"));
				proposal.setActivityTypeCode(resultSet.getString("ACTIVITY_TYPE_CODE"));
				proposal.setApplicationActivityType(resultSet.getString("ACTIVITY_TYPE"));
				proposal.setIpNumber(resultSet.getString("IP_NUMBER")); 
				proposal.setSubmissionDate(resultSet.getTimestamp("SUBMISSION_DATE"));
				proposal.setHomeUnitNumber(resultSet.getString("HOME_UNIT_NUMBER"));
				proposal.setCreateUser(resultSet.getString("EPS_PROPOSAL_CREATE_USER"));
				if (resultSet.getString("GRANT_TYPE_CODE") != null) {
					proposal.setGrantTypeCode(Integer.parseInt(resultSet.getString("GRANT_TYPE_CODE")));
				}
				if (resultSet.getString("PROPOSAL_RANK") != null) {
					proposal.setProposalRank(Integer.parseInt(resultSet.getString("PROPOSAL_RANK")));
				}
				if (resultSet.getString("EVALUATION_RECOMMENDATION_CODE") != null) {
					proposal.setRecommendationCode(Integer.parseInt(resultSet.getString("EVALUATION_RECOMMENDATION_CODE")));
				}
				if (resultSet.getString("GRANT_CALL_CATEGORY_CODE") != null) {
					proposal.setCategoryCode(Integer.parseInt(resultSet.getString("GRANT_CALL_CATEGORY_CODE")));
				}
				if (resultSet.getString("SUBMIT_USER") != null) {
					proposal.setSubmitUserFullName(personDao.getUserFullNameByUserName(resultSet.getString("SUBMIT_USER")));
				}
//				This code is intentionally commented to speed up the data loading - by Arjun
//				if(vo.getPersonId() != null) {
//					proposal.setHasRank(evaluationDao.checkPersonHasRank(proposal.getProposalId(), vo.getPersonId()));
//					proposal.setHasRecommendation(evaluationDao.checkPersonHasRecommendation(proposal.getProposalId(), vo.getPersonId()));
//				}
				if(resultSet.getString("ABBREVIATION") != null) {
					proposal.setAbbreviation(resultSet.getString("ABBREVIATION"));
				}
				proposal.setInternalDeadLineDate(resultSet.getTimestamp("INTERNAL_DEADLINE_DATE"));
//				List<ProposalReview> proposalReviews = proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposal.getProposalId());
//				if (proposalReviews != null) {
//					for (ProposalReview proposalReview : proposalReviews) {
//						if (proposalReview.getRoleId() == Constants.PROPOSAL_GRANT_MANAGER_ROLE_TYPE_CODE) {
//							proposal.getGrantORTTManagers().add(proposalReview.getReviewerFullName());
//						}
//					}
//				}
//				if (proposal.getStatusCode() == Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS
//						|| proposal.getStatusCode() == Constants.PROPOSAL_STATUS_CODE_RETURNED
//						|| proposal.getStatusCode() == Constants.PROPOSAL_STATUS_CODE_AWARDED
//						|| (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE)
//								.equals(Constants.EVALUATION_MAP_ROUTING)
//								&& (proposal.getStatusCode() != Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS
//										|| proposal.getStatusCode() != Constants.PROPOSAL_STATUS_CODE_RETURNED))) {
//					Workflow workflow = workflowDao.fetchActiveWorkflowByParams(proposal.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
//					if (workflow == null) {
//						workflow = new Workflow();
//					}
//					workflowService.prepareWorkflowDetails(workflow);
//					proposal.setWorkflow(workflow);
//					List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(proposal.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
//					if (workFlows != null && !workFlows.isEmpty()) {
//						workflowService.prepareWorkflowDetailsList(workFlows);
//						Collections.sort(workFlows, new WorkflowComparator());
//						proposal.setWorkflowList(workFlows);
//					}
//				}
				proposals.add(proposal);
			}
			dashBoardProfile.setProposal(proposals);
			dashBoardProfile.setTotalServiceRequest(getDashboardProposalsCount(vo));
			dashBoardProfile.setFinalEvaluationStatus(evaluationDao.getFinalEvaluationStatus());
		} catch (SQLException e) {
			logger.info("exception in dashboardProposals : " + e);
			e.printStackTrace();
		}
		return dashBoardProfile;
	}

	private Integer getDashboardProposalsCount(ProposalDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer count = 0;
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		List<String> property3 = vo.getProperty3();
		List<String> property4 = vo.getProperty4();
		String property5 = vo.getProperty5();
		List<String> property6 = vo.getProperty6();
		String property7 = vo.getProperty7();
		String property8 = vo.getProperty8();
		String property9 = vo.getProperty9();
		String property10 = vo.getProperty10();
		String property11 = vo.getProperty11();
		String property12 = vo.getProperty12();
		List<String> propertyList13 = vo.getProperty13();
		String property13 = String.join(",", propertyList13);
		String property14 = vo.getProperty14();
		String property15 = vo.getProperty15();
		String property16 = vo.getProperty16();
		String personId = vo.getPersonId();
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_PROPOSAL_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, property1);
				statement.setString(2, property2);
				statement.setString(3, property5);
				statement.setString(4, String.join(",", property4));
				statement.setString(5, String.join(",", property6));
				statement.setString(6, property7);
				statement.setString(7, property8);
				statement.setString(8, String.join(",", property3));
				statement.setString(9, property10);
				statement.setString(10, property9);
				statement.setString(11, property11);
				statement.setString(12, personId);
				statement.setString(13, setOrder(sort));
				statement.setInt(14, 0);
				statement.setInt(15, 0);
				statement.setString(16, tabName);
				statement.setBoolean(17, true);
				statement.setString(18, isAdvancedSearch);
				statement.setString(19, property12);
				statement.setString(20, property13);
				statement.setString(21, property14);
				statement.setString(22, property15);
				statement.setString(23, property16);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_PROPOSAL_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, property1);
				statement.setString(3, property2);
				statement.setString(4, property5);
				statement.setString(5, String.join(",", property4));
				statement.setString(6, String.join(",", property6));
				statement.setString(7, property7);
				statement.setString(8, property8);
				statement.setString(9, String.join(",", property3));
				statement.setString(10, property10);
				statement.setString(11, property9);
				statement.setString(12, property11);
				statement.setString(13, personId);
				statement.setString(14, setOrder(sort));
				statement.setInt(15, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(16, (pageNumber == null ? 0 : pageNumber));
				statement.setString(17, tabName);
				statement.setBoolean(18, true);
				statement.setString(19, isAdvancedSearch);
				statement.setString(20, property12);
				statement.setString(21, property13);
				statement.setString(22, property14);
				statement.setString(23, property15);
				statement.setString(24, property16);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.info("exception in dashboardProposals : " + e);
			e.printStackTrace();
		}
		return count;
	}

	private String setOrder(Map<String, String> sort) {
		String sortOrder = null;
		if (!sort.isEmpty()) {
			for (Map.Entry<String, String> mapElement : sort.entrySet()) {
				if (mapElement.getKey().equals("proposalPersons.fullName")) {
					sortOrder = (sortOrder == null ? "T.FULL_NAME " + mapElement.getValue() : sortOrder + ", T.FULL_NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("proposalId")) {
					sortOrder = (sortOrder == null ? "T.PROPOSAL_ID " + mapElement.getValue() : sortOrder + ", T.PROPOSAL_ID " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("grantCallClosingDate")) {
					sortOrder = (sortOrder == null ? "T.CLOSING_DATE " + mapElement.getValue() : sortOrder + ", T.CLOSING_DATE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("grantCallName")) {
					sortOrder = (sortOrder == null ? "T.NAME " + mapElement.getValue() : sortOrder + ", T.NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("homeUnitName")) {
					sortOrder = (sortOrder == null ? "T.HOME_UNIT_NAME " + mapElement.getValue() : sortOrder + ", T.HOME_UNIT_NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("proposalStatus.description")) {
					sortOrder = (sortOrder == null ? "T.PROPOSAL_STATUS " + mapElement.getValue() : sortOrder + ", T.PROPOSAL_STATUS " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("sponsorName")) {
					sortOrder = (sortOrder == null ? "T.SPONSOR_NAME " + mapElement.getValue() : sortOrder + ", T.SPONSOR_NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("title")) {
					sortOrder = (sortOrder == null ? "T.TITLE " + mapElement.getValue() : sortOrder + ", T.TITLE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("applicationId")) {
					sortOrder = (sortOrder == null ? "T.APPLICATION_ID " + mapElement.getValue() : sortOrder + ", T.APPLICATION_ID " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("activityType")) {
					sortOrder = (sortOrder == null ? "T.ACTIVITY_TYPE " + mapElement.getValue() : sortOrder + ", T.ACTIVITY_TYPE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("abbreviation")) {
					sortOrder = (sortOrder == null ? "T.ABBREVIATION " + mapElement.getValue() : sortOrder + ", T.ABBREVIATION " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("sponsorDeadlineDate")) {
					sortOrder = (sortOrder == null ? "T.SPONSOR_DEADLINE_DATE " + mapElement.getValue() : sortOrder + ", T.SPONSOR_DEADLINE_DATE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("proposalType.description")) {
					sortOrder = (sortOrder == null ? "T.PROPOSAL_TYPE " + mapElement.getValue() : sortOrder + ", T.PROPOSAL_TYPE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("internalDeadlineDate")) {
					sortOrder = (sortOrder == null ? "T.INTERNAL_DEADLINE_DATE " + mapElement.getValue() : sortOrder + ", T.INTERNAL_DEADLINE_DATE " + mapElement.getValue());
				}
			}
		}
		return sortOrder;
	}

	@Override
	public DashBoardProfile dashboardDatasForAward(AwardDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<AwardView> awardViews = new ArrayList<>();
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		String property3 = vo.getProperty3();
		String property4 = vo.getProperty4();
		String property5 = vo.getProperty5();
		String property6 = vo.getProperty6();
		String property7 = vo.getProperty7();
		List<String> property8 = vo.getProperty8();
		String property9 = vo.getProperty9();
		String property10 = vo.getProperty10();
		List<String> property11 = vo.getProperty11();
		String property12 = vo.getProperty12();
		List<String> property13 = vo.getProperty13();
		List<String> property14 = vo.getProperty14();
		List<String> property15 = vo.getProperty15();
		String personId = vo.getPersonId();
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_AWARD_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, property5);
				statement.setString(2, property6);
				statement.setString(3, property1);
				statement.setString(4, property7);
				statement.setString(5, property2);
				statement.setString(6, property3);
				statement.setString(7, property4);
				statement.setString(8, String.join(",", property8));
				statement.setString(9, property10);
				statement.setString(10, property9);
				statement.setString(11, personId);
				statement.setString(12, setAwardSortOrder(sort));
				statement.setInt(13, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(14, (pageNumber == null ? 0 : pageNumber));
				statement.setString(15, tabName);
				statement.setBoolean(16, isDownload);
				statement.setString(17, isAdvancedSearch);
				statement.setString(18, String.join(",", property11));
				statement.setString(19, property12);
				statement.setString(20, String.join(",", property13));
				statement.setString(21, String.join(",", property14));
				statement.setString(22, String.join(",", property15));
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_AWARD_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, property5);
				statement.setString(3, property6);
				statement.setString(4, property1);
				statement.setString(5, property7);
				statement.setString(6, property2);
				statement.setString(7, property3);
				statement.setString(8, property4);
				statement.setString(9, String.join(",", property8));
				statement.setString(10, property10);
				statement.setString(11, property9);
				statement.setString(12, personId);
				statement.setString(13, setAwardSortOrder(sort));
				statement.setInt(14, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(15, (pageNumber == null ? 0 : pageNumber));
				statement.setString(16, tabName);
				statement.setBoolean(17, isDownload);
				statement.setString(18, isAdvancedSearch);
				statement.setString(19, String.join(",", property11));
				statement.setString(20, property12);
				statement.setString(21, String.join(",", property13));
				statement.setString(22, String.join(",", property14));
				statement.setString(23, String.join(",", property15));
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				AwardView awardView = new AwardView();
				awardView.setAccountNumber(resultSet.getString("ACCOUNT_NUMBER"));
				awardView.setAwardId(resultSet.getInt("AWARD_ID"));
				awardView.setAwardNumber(resultSet.getString("AWARD_NUMBER"));
				awardView.setFullName(resultSet.getString("FULL_NAME"));
				awardView.setTitle(resultSet.getString("TITLE"));
				awardView.setUnitName(resultSet.getString("UNIT_NAME"));
				awardView.setUnitNumber(resultSet.getString("LEAD_UNIT_NUMBER"));
				awardView.setSponsorCode(resultSet.getString("SPONSOR_CODE"));
				awardView.setSponsor(commonService.getSponsorFormatBySponsorDetail(resultSet.getString("SPONSOR_CODE"), resultSet.getString("SPONSOR_NAME"), resultSet.getString("ACRONYM")));
				awardView.setStatus(resultSet.getString("AWARD_STATUS"));
				awardView.setAwardType(resultSet.getString("AWARD_TYPE"));
				awardView.setAwardDocumentType(resultSet.getString("AWARD_DOCUMENT_TYPE"));
				awardView.setGrantCallTitle(resultSet.getString("NAME"));
				awardView.setWorkflowAwardStatusCode(resultSet.getString("WORKFLOW_AWARD_STATUS_CODE"));
				awardView.setAwardWorkflowStatus(resultSet.getString("AWARD_WORKFLOW_STATUS"));
				awardView.setAwardVariationType(resultSet.getString("AWARD_VARIATION_TYPE"));
				awardView.setSponsorAwardNumber(resultSet.getString("SPONSOR_AWARD_NUMBER"));
				awardView.setAwardSequenceStatus(resultSet.getString("AWARD_SEQUENCE_STATUS"));
				awardViews.add(awardView);
			}
			dashBoardProfile.setAwardViews(awardViews);
			dashBoardProfile.setTotalServiceRequest(getDashboardAwardsCount(vo));
		} catch (SQLException e) {
			logger.info("exception in dashboardAward : " + e);
			e.printStackTrace();
		}
		return dashBoardProfile;
	}

	private String setAwardSortOrder(Map<String, String> sort) {
		String sortOrder = null;
		if (!sort.isEmpty()) {
			for (Map.Entry<String, String> mapElement : sort.entrySet()) {
				if (mapElement.getKey().equals("accountNumber")) {
					sortOrder = (sortOrder == null ? "T.ACCOUNT_NUMBER " + mapElement.getValue() : sortOrder + ", T.ACCOUNT_NUMBER " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("awardNumber")) {
					sortOrder = (sortOrder == null ? "T.AWARD_NUMBER " + mapElement.getValue() : sortOrder + ", T.AWARD_NUMBER " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("awardPersons.fullName")) {
					sortOrder = (sortOrder == null ? "T.FULL_NAME " + mapElement.getValue() : sortOrder + ", T.FULL_NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("awardStatus.description")) {
					sortOrder = (sortOrder == null ? "T.AWARD_STATUS " + mapElement.getValue() : sortOrder + ", T.AWARD_STATUS " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("leadUnit.unitName")) {
					sortOrder = (sortOrder == null ? "T.UNIT_NAME " + mapElement.getValue() : sortOrder + ", T.UNIT_NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("sponsor.sponsorName")) {
					sortOrder = (sortOrder == null ? "T.SPONSOR_NAME " + mapElement.getValue() : sortOrder + ", T.SPONSOR_NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("title")) {
					sortOrder = (sortOrder == null ? "T.TITLE " + mapElement.getValue() : sortOrder + ", T.TITLE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("awardType")) {
					sortOrder = (sortOrder == null ? "T.AWARD_TYPE " + mapElement.getValue() : sortOrder + ", T.AWARD_TYPE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("awardDocumentType")) {
					sortOrder = (sortOrder == null ? "T.AWARD_DOCUMENT_TYPE " + mapElement.getValue() : sortOrder + ", T.AWARD_DOCUMENT_TYPE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("awardWorkflowStatus")) {
					sortOrder = (sortOrder == null ? "T.AWARD_WORKFLOW_STATUS " + mapElement.getValue() : sortOrder + ", T.AWARD_WORKFLOW_STATUS " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("awardVariationType")) {
					sortOrder = (sortOrder == null ? "T.AWARD_VARIATION_TYPE " + mapElement.getValue() : sortOrder + ", T.AWARD_VARIATION_TYPE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("awardSequenceStatus")) {
					sortOrder = (sortOrder == null ? "T.AWARD_SEQUENCE_STATUS " + mapElement.getValue() : sortOrder + ", T.AWARD_SEQUENCE_STATUS " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("sponsorAwardNumber")) {
					sortOrder = (sortOrder == null ? "T.SPONSOR_AWARD_NUMBER " + mapElement.getValue() : sortOrder + ", T.SPONSOR_AWARD_NUMBER " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("claimId")) {
					sortOrder = (sortOrder == null ? "T.CLAIM_ID " + mapElement.getValue() : sortOrder + ", T.CLAIM_ID " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("claimSubmissionDate")) {
					sortOrder = (sortOrder == null ? "T.CLAIM_SUBMISSION_DATE " + mapElement.getValue() : sortOrder + ", T.CLAIM_SUBMISSION_DATE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("createUserName")) {
					sortOrder = (sortOrder == null ? "T.CREATE_USERNAME " + mapElement.getValue() : sortOrder + ", T.CREATE_USERNAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("updateUserName")) {
					sortOrder = (sortOrder == null ? "T.UPDATE_USERNAME " + mapElement.getValue() : sortOrder + ", T.UPDATE_USERNAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("claimUpdateTimeStamp")) {
					sortOrder = (sortOrder == null ? "T.CLAIM_UPDATE_TIMESTAMP " + mapElement.getValue() : sortOrder + ", T.CLAIM_UPDATE_TIMESTAMP " + mapElement.getValue());
				}
			}
		}
		return sortOrder;
	}

	private Integer getDashboardAwardsCount(AwardDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer count = 0;
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		String property3 = vo.getProperty3();
		String property4 = vo.getProperty4();
		String property5 = vo.getProperty5();
		String property6 = vo.getProperty6();
		String property7 = vo.getProperty7();
		List<String> property = vo.getProperty8();
		String property8 =  String.join(",", property);
		String property9 = vo.getProperty9();
		String property10 = vo.getProperty10();
		List<String> property11 = vo.getProperty11();
		String personId = vo.getPersonId();
		String tabName = vo.getTabName();
		String isAdvancedSearch = vo.getAdvancedSearch();
		String property12 = vo.getProperty12();
		List<String> propertysr = vo.getProperty13();
		String property13 =  String.join(",", propertysr);
		List<String> propertyat = vo.getProperty14();
		String property14 =  String.join(",", propertyat);
		List<String> propertyGrantType = vo.getProperty15();
		String property15 =  String.join(",", propertyGrantType);
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_AWARD_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, property5);
				statement.setString(2, property6);
				statement.setString(3, property1);
				statement.setString(4, property7);
				statement.setString(5, property2);
				statement.setString(6, property3);
				statement.setString(7, property4);
				statement.setString(8, property8);
				statement.setString(9, property10);
				statement.setString(10, property9);
				statement.setString(11, personId);
				statement.setString(12, setAwardSortOrder(sort));
				statement.setInt(13, 0);
				statement.setInt(14, 0);
				statement.setString(15, tabName);
				statement.setBoolean(16, true);
				statement.setString(17, isAdvancedSearch);
				statement.setString(18, String.join(",", property11));
				statement.setString(19, property12);
				statement.setString(20, property13);
				statement.setString(21, property14);
				statement.setString(22, property15);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_AWARD_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, property5);
				statement.setString(3, property6);
				statement.setString(4, property1);
				statement.setString(5, property7);
				statement.setString(6, property2);
				statement.setString(7, property3);
				statement.setString(8, property4);
				statement.setString(9, property8);
				statement.setString(10, property10);
				statement.setString(11, property9);
				statement.setString(12, personId);
				statement.setString(13, setAwardSortOrder(sort));
				statement.setInt(14, 0);
				statement.setInt(15, 0);
				statement.setString(16, tabName);
				statement.setBoolean(17, true);
				statement.setString(18, isAdvancedSearch);
				statement.setString(19, String.join(",", property11));
				statement.setString(20, property12);
				statement.setString(21, property13);
				statement.setString(22, property14);
				statement.setString(23, property15);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.info("exception in dashboardAwards : " + e);
			e.printStackTrace();
		}
		return count;
	}

	@Override
	public DashBoardProfile loadServiceRequestDashBoard(ServiceRequestDashboardVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		dashBoardProfile.setServiceRequestList(getDashboardListForServiceRequest(vo));
		if (vo.getIsDownload().equals(false)) {
			dashBoardProfile.setTotalServiceRequest(getDashboardCountForServiceRequest(vo));
		}
		return dashBoardProfile;
	}

	private Integer getDashboardCountForServiceRequest(ServiceRequestDashboardVO vo) {
		Integer count = null;
		vo.setProcedureName("GET_SERVICE_REQUEST_DASHBOARD_COUNT");
		vo.setCurrentPage(1);
		try {
			ResultSet resultSet = getDashboardCountAndListForServiceRequest(vo);
			while (resultSet != null && resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.info("Exception in Sevice request dashboard : {}", e.getMessage());
			e.printStackTrace();
		}
		return count;
	}

	private List<ServiceRequest> getDashboardListForServiceRequest(ServiceRequestDashboardVO vo) {
		vo.setProcedureName("GET_SERVICE_REQUEST_DASHBOARD");
 		List<ServiceRequest> serviceRequests = new ArrayList<>();
		try {
			ResultSet resultSet = getDashboardCountAndListForServiceRequest(vo);
			while (resultSet != null && resultSet.next()) {
				ServiceRequest serviceRequest = new ServiceRequest();
				serviceRequest.setServiceRequestId(resultSet.getInt("SR_HEADER_ID"));
				serviceRequest.setsRPriority(resultSet.getString("PRIORITY"));
				serviceRequest.setServiceRequestCategory(resultSet.getString("CATEGORY"));
				serviceRequest.setServiceRequestTypeData(resultSet.getString("REQUEST_TYPE"));
				serviceRequest.setSubject(resultSet.getString("SUBJECT"));
				serviceRequest.setUnitName(resultSet.getString("UNIT_NAME"));
				serviceRequest.setUnitNumber(resultSet.getString("UNIT_NUMBER"));
				serviceRequest.setUpdateTimestamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				serviceRequest.setServiceRequestStatusData(resultSet.getString("REQUEST_STATUS"));
				serviceRequests.add(serviceRequest);
			}
		} catch (SQLException e) {
			logger.info("Exception in Sevice request dashboard : {}", e.getMessage());
			e.printStackTrace();
		}
		return serviceRequests;
	}

	private ResultSet getDashboardCountAndListForServiceRequest(ServiceRequestDashboardVO vo) throws SQLException {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		String procedureName = vo.getProcedureName();
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, vo.getServiceRequestId());
				statement.setString(2, vo.getServiceRequestSubject());
				statement.setString(3, String.join(",", vo.getModuleCodes()));
				statement.setString(4, String.join(",", vo.getSrTypeCodes()));
				statement.setString(5, String.join(",", vo.getSrStatusCodes()));
				statement.setString(6, vo.getUnitNumber());
				statement.setString(7, String.join(",", vo.getSrPriorities()));
				statement.setString(8, vo.getPersonId());
				statement.setString(9, setServiceRrequestSortOrder(sort));
				statement.setInt(10, (vo.getCurrentPage() == null ? 0 : vo.getCurrentPage() - 1));
				statement.setInt(11, (vo.getPageNumber() == null ? 0 : vo.getPageNumber()));
				statement.setString(12, vo.getTabName());
				statement.setBoolean(13, vo.getIsDownload());
				statement.setString(14, vo.getAdvancedSearch());
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, vo.getServiceRequestId());
				statement.setString(3, vo.getServiceRequestSubject());
				statement.setString(4, String.join(",", vo.getModuleCodes()));
				statement.setString(5, String.join(",", vo.getSrTypeCodes()));
				statement.setString(6, String.join(",", vo.getSrStatusCodes()));
				statement.setString(7, vo.getUnitNumber());
				statement.setString(8, String.join(",", vo.getSrPriorities()));
				statement.setString(9, vo.getPersonId());
				statement.setString(10, setServiceRrequestSortOrder(sort));
				statement.setInt(11, (vo.getCurrentPage() == null ? 0 : vo.getCurrentPage() - 1));
				statement.setInt(12, (vo.getPageNumber() == null ? 0 : vo.getPageNumber()));
				statement.setString(13, vo.getTabName());
				statement.setBoolean(14, vo.getIsDownload());
				statement.setString(15, vo.getAdvancedSearch());
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
		} catch (SQLException e) {
			logger.info("Exception in Sevice request dashboard call : {}", e.getMessage());
			e.printStackTrace();
		}
		return resultSet;
	}

	private String setServiceRrequestSortOrder(Map<String, String> sort) {
		String sortOrder = null;
		if (!sort.isEmpty()) {
			for (Map.Entry<String, String> mapElement : sort.entrySet()) {
				switch (mapElement.getKey()) {
				case "serviceRequestId":
					sortOrder = (sortOrder == null ? "T.SR_HEADER_ID " + mapElement.getValue()
							: sortOrder + ", T.SR_HEADER_ID " + mapElement.getValue());
					break;
				case "serviceRequestCategory":
					sortOrder = (sortOrder == null ? "T.CATEGORY " + mapElement.getValue()
							: sortOrder + ", T.CATEGORY " + mapElement.getValue());
					break;
				case "unitName":
					sortOrder = (sortOrder == null ? "T.UNIT_NAME " + mapElement.getValue()
							: sortOrder + ", T.UNIT_NAME " + mapElement.getValue());
					break;
				case "serviceRequestType":
					sortOrder = (sortOrder == null ? "T.REQUEST_TYPE " + mapElement.getValue()
							: sortOrder + ", T.REQUEST_TYPE " + mapElement.getValue());
					break;
				case "subject":
					sortOrder = (sortOrder == null ? "T.SUBJECT " + mapElement.getValue()
							: sortOrder + ", T.SUBJECT " + mapElement.getValue());
					break;
				case "updateTimeStamp":
					sortOrder = (sortOrder == null ? "T.UPDATE_TIMESTAMP " + mapElement.getValue()
							: sortOrder + ", T.UPDATE_TIMESTAMP " + mapElement.getValue());
					break;
				case "serviceRequestStatusData":
					sortOrder = (sortOrder == null ? "T.REQUEST_STATUS " + mapElement.getValue()
							: sortOrder + ", T.REQUEST_STATUS " + mapElement.getValue());
					break;
				case "sRPriority":
					sortOrder = (sortOrder == null ? "T.PRIORITY " + mapElement.getValue()
							: sortOrder + ", T.PRIORITY " + mapElement.getValue());
					break;
				default:
					break;
				}
			}
		}
		return sortOrder;
	}

	@Override
	public List<FileType> getAllFileTypes() {
		return hibernateTemplate.loadAll(FileType.class);
	}

	@Override
	public DashBoardProfile getClaimDashBoardData(ClaimDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<AwardView> awardViews = new ArrayList<>();
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		String property3 = vo.getProperty3();
		String property4 = vo.getProperty4();
		String property5 = vo.getProperty5();
		String property6 = vo.getProperty6();
		String property7 = vo.getProperty7();
		List<String> property8 = vo.getProperty8();
		String property9 = vo.getProperty9();
		String property10 = vo.getProperty10();
		List<String> property11 = vo.getProperty11();
		String property12 = vo.getProperty12();
		String personId = vo.getPersonId();
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		String property13 = vo.getProperty13();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_CLAIM_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, property5);
				statement.setString(2, property6);
				statement.setString(3, property1);
				statement.setString(4, property7);
				statement.setString(5, property2);
				statement.setString(6, property3);
				statement.setString(7, property4);
				statement.setString(8, String.join(",", property8));
				statement.setString(9, property10);
				statement.setString(10, property9);
				statement.setString(11, personId);
				statement.setString(12, setClaimSortOrder(sort));
				statement.setInt(13, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(14, (pageNumber == null ? 0 : pageNumber));
				statement.setString(15, tabName);
				statement.setBoolean(16, isDownload);
				statement.setString(17, isAdvancedSearch);
				statement.setString(18, String.join(",", property11));
				statement.setString(19, property12);
				statement.setString(20, property13);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_CLAIM_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, property5);
				statement.setString(3, property6);
				statement.setString(4, property1);
				statement.setString(5, property7);
				statement.setString(6, property2);
				statement.setString(7, property3);
				statement.setString(8, property4);
				statement.setString(9, String.join(",", property8));
				statement.setString(10, property10);
				statement.setString(11, property9);
				statement.setString(12, personId);
				statement.setString(13, setClaimSortOrder(sort));
				statement.setInt(14, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(15, (pageNumber == null ? 0 : pageNumber));
				statement.setString(16, tabName);
				statement.setBoolean(17, isDownload);
				statement.setString(18, isAdvancedSearch);
				statement.setString(19, String.join(",", property11));
				statement.setString(20, property12);
				statement.setString(21, property13);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				AwardView awardView = new AwardView();
				awardView.setAccountNumber(resultSet.getString("ACCOUNT_NUMBER"));
				awardView.setAwardId(resultSet.getInt("AWARD_ID"));
				awardView.setAwardNumber(resultSet.getString("AWARD_NUMBER"));
				awardView.setFullName(resultSet.getString("FULL_NAME"));
				awardView.setTitle(resultSet.getString("TITLE"));
				awardView.setUnitName(resultSet.getString("UNIT_NAME"));
				awardView.setSponsor(commonService.getSponsorFormatBySponsorDetail(resultSet.getString("SPONSOR_CODE"), resultSet.getString("SPONSOR_NAME"), resultSet.getString("ACRONYM")));
				awardView.setSponsorAwardNumber(resultSet.getString("SPONSOR_AWARD_NUMBER"));
				awardView.setAwardSequenceStatus(resultSet.getString("AWARD_SEQUENCE_STATUS"));
				awardView.setClaimId(resultSet.getInt("CLAIM_ID"));
				awardView.setClaimNumber(resultSet.getString("CLAIM_NUMBER"));
				awardView.setClaimSubmissionDate(resultSet.getDate("CLAIM_SUBMISSION_DATE") != null ? new Date(resultSet.getDate("CLAIM_SUBMISSION_DATE").getTime()) : null);
				awardView.setCreateUserName(resultSet.getString("CREATE_USERNAME"));
				awardView.setUpdateUserName(resultSet.getString("UPDATE_USERNAME"));
				awardView.setClaimUpdateTimeStamp(resultSet.getDate("CLAIM_UPDATE_TIMESTAMP") != null ? new Date(resultSet.getDate("CLAIM_UPDATE_TIMESTAMP").getTime()) : null);
				awardView.setAwardStartDate(resultSet.getDate("AWARD_START_DATE") != null ? new Date(resultSet.getDate("AWARD_START_DATE").getTime()) : null);
				awardView.setAwardEndDate(resultSet.getDate("AWARD_END_DATE") != null ? new Date(resultSet.getDate("AWARD_END_DATE").getTime()) : null);
				awardView.setLastClaimEndDate(resultSet.getDate("CLAIM_END_DATE") != null ? new Date(resultSet.getDate("CLAIM_END_DATE").getTime()) : null);
				awardView.setFinanceOfficer(resultSet.getString("FINANCE_OFFICER"));
				awardView.setClaimStatus(resultSet.getString("CLAIM_STATUS"));
				awardView.setClaimStatusCode(resultSet.getString("CLAIM_STATUS_CODE"));
				awardView.setUnitNumber(resultSet.getString("LEAD_UNIT_NUMBER"));
				awardViews.add(awardView);
			}
			dashBoardProfile.setAwardViews(awardViews);
			dashBoardProfile.setTotalServiceRequest(getClaimDashboardCount(vo));
		} catch (SQLException e) {
			logger.error("Error in getClaimDashBoardData {}", e.getMessage());
		}
		return dashBoardProfile;
	}
	
	private Integer getClaimDashboardCount(ClaimDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer count = 0;
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		String property3 = vo.getProperty3();
		String property4 = vo.getProperty4();
		String property5 = vo.getProperty5();
		String property6 = vo.getProperty6();
		String property7 = vo.getProperty7();
		List<String> property = vo.getProperty8();
		String property8 =  String.join(",", property);
		String property9 = vo.getProperty9();
		String property10 = vo.getProperty10();
		List<String> property11 = vo.getProperty11();
		String personId = vo.getPersonId();
		String tabName = vo.getTabName();
		String isAdvancedSearch = vo.getAdvancedSearch();
		String property12 = vo.getProperty12();
		String property13 = vo.getProperty13();
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_CLAIM_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, property5);
				statement.setString(2, property6);
				statement.setString(3, property1);
				statement.setString(4, property7);
				statement.setString(5, property2);
				statement.setString(6, property3);
				statement.setString(7, property4);
				statement.setString(8, property8);
				statement.setString(9, property10);
				statement.setString(10, property9);
				statement.setString(11, personId);
				statement.setString(12, setClaimSortOrder(sort));
				statement.setInt(13, 0);
				statement.setInt(14, 0);
				statement.setString(15, tabName);
				statement.setBoolean(16, true);
				statement.setString(17, isAdvancedSearch);
				statement.setString(18, String.join(",", property11));
				statement.setString(19, property12);
				statement.setString(20, property13);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_CLAIM_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, property5);
				statement.setString(3, property6);
				statement.setString(4, property1);
				statement.setString(5, property7);
				statement.setString(6, property2);
				statement.setString(7, property3);
				statement.setString(8, property4);
				statement.setString(9, property8);
				statement.setString(10, property10);
				statement.setString(11, property9);
				statement.setString(12, personId);
				statement.setString(13, setClaimSortOrder(sort));
				statement.setInt(14, 0);
				statement.setInt(15, 0);
				statement.setString(16, tabName);
				statement.setBoolean(17, true);
				statement.setString(18, isAdvancedSearch);
				statement.setString(19, String.join(",", property11));
				statement.setString(20, property12);
				statement.setString(21, property13);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.error("Error in getClaimDashboardCount {}", e.getMessage());
		}
		return count;
	}
	
	private String setClaimSortOrder(Map<String, String> sort) {
		String sortOrder = null;
		if (!sort.isEmpty()) {
			for (Map.Entry<String, String> mapElement : sort.entrySet()) {
				if (mapElement.getKey().equals("accountNumber")) {
					sortOrder = (sortOrder == null ? "T.ACCOUNT_NUMBER " + mapElement.getValue() : sortOrder + ", T.ACCOUNT_NUMBER " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("awardNumber")) {
					sortOrder = (sortOrder == null ? "T.AWARD_NUMBER " + mapElement.getValue() : sortOrder + ", T.AWARD_NUMBER " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("awardPersons.fullName")) {
					sortOrder = (sortOrder == null ? "T.FULL_NAME " + mapElement.getValue() : sortOrder + ", T.FULL_NAME " + mapElement.getValue());
				}		
				if (mapElement.getKey().equals("leadUnit.unitName")) {
					sortOrder = (sortOrder == null ? "T.UNIT_NAME " + mapElement.getValue() : sortOrder + ", T.UNIT_NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("sponsor.sponsorName")) {
					sortOrder = (sortOrder == null ? "SPONSOR_NAME " + mapElement.getValue() : sortOrder + ", SPONSOR_NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("title")) {
					sortOrder = (sortOrder == null ? "T.TITLE " + mapElement.getValue() : sortOrder + ", T.TITLE " + mapElement.getValue());
				}				
				if (mapElement.getKey().equals("awardSequenceStatus")) {
					sortOrder = (sortOrder == null ? "T.AWARD_SEQUENCE_STATUS " + mapElement.getValue() : sortOrder + ", T.AWARD_SEQUENCE_STATUS " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("sponsorAwardNumber")) {
					sortOrder = (sortOrder == null ? "T.SPONSOR_AWARD_NUMBER " + mapElement.getValue() : sortOrder + ", T.SPONSOR_AWARD_NUMBER " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("claimNumber")) {
					sortOrder = (sortOrder == null ? "T.CLAIM_NUMBER " + mapElement.getValue() : sortOrder + ", T.CLAIM_NUMBER " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("claimSubmissionDate")) {
					sortOrder = (sortOrder == null ? "T.CLAIM_SUBMISSION_DATE " + mapElement.getValue() : sortOrder + ", T.CLAIM_SUBMISSION_DATE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("createUserName")) {
					sortOrder = (sortOrder == null ? "T.CREATE_USERNAME " + mapElement.getValue() : sortOrder + ", T.CREATE_USERNAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("updateUserName")) {
					sortOrder = (sortOrder == null ? "T.UPDATE_USERNAME " + mapElement.getValue() : sortOrder + ", T.UPDATE_USERNAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("claimUpdateTimeStamp")) {
					sortOrder = (sortOrder == null ? "T.CLAIM_UPDATE_TIMESTAMP " + mapElement.getValue() : sortOrder + ", T.CLAIM_UPDATE_TIMESTAMP " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("financeOfficer")) {
					sortOrder = (sortOrder == null ? "T.FINANCE_OFFICER " + mapElement.getValue() : sortOrder + ", T.FINANCE_OFFICER " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("claimStatus.description")) {
					sortOrder = (sortOrder == null ? "T.CLAIM_STATUS " + mapElement.getValue() : sortOrder + ", T.CLAIM_STATUS " + mapElement.getValue());
				}
			}
		}
		return sortOrder;
	}

	@Override
	public DashBoardProfile fibiProgressReportDashBoard(ProgressReportDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<AwardView> awardViews = new ArrayList<>();
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		String property3 = vo.getProperty3();
		String property4 = vo.getProperty4();
		String property5 = vo.getProperty5();
		String personId = vo.getPersonId();
		String dueDateString = vo.getProperty13();		
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_PROGRESS_REPORT_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, property1);
				statement.setString(2, property2);
				statement.setString(3, property3);
				statement.setString(4, property4);
				statement.setString(5, personId);
				statement.setString(6, dueDateString);
				statement.setString(7, setProgressReportSortOrder(sort));
				statement.setInt(8, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(9, (pageNumber == null ? 0 : pageNumber));
				statement.setString(10, tabName);
				statement.setBoolean(11, isDownload);
				statement.setString(12, isAdvancedSearch);
				statement.setString(13, property5);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_PROGRESS_REPORT_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, property1);
				statement.setString(3, property2);
				statement.setString(4, property3);
				statement.setString(5, property4);
				statement.setString(6, personId);
				statement.setString(7, dueDateString);
				statement.setString(8, setProgressReportSortOrder(sort));
				statement.setInt(9, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(10, (pageNumber == null ? 0 : pageNumber));
				statement.setString(11, tabName);
				statement.setBoolean(12, isDownload);
				statement.setString(13, isAdvancedSearch);
				statement.setString(14, property5);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				AwardView awardView = new AwardView();
				awardView.setAccountNumber(resultSet.getString("ACCOUNT_NUMBER"));
				awardView.setAwardId(resultSet.getInt("AWARD_ID"));
				awardView.setAwardNumber(resultSet.getString("AWARD_NUMBER"));
				awardView.setFullName(resultSet.getString("FULL_NAME"));
				awardView.setTitle(resultSet.getString("TITLE"));
				awardView.setUnitName(resultSet.getString("UNIT_NAME"));
				awardView.setUnitNumber(resultSet.getString("UNIT_NUMBER"));
				awardView.setSponsor(commonService.getSponsorFormatBySponsorDetail(resultSet.getString("SPONSOR_CODE"), resultSet.getString("SPONSOR_NAME"), resultSet.getString("ACRONYM")));
				awardView.setSponsorAwardNumber(resultSet.getString("SPONSOR_AWARD_NUMBER"));
				awardView.setAwardSequenceStatus(resultSet.getString("AWARD_SEQUENCE_STATUS"));
				awardView.setProgressReportId(resultSet.getInt("PROGRESS_REPORT_ID"));
				awardView.setProgressReportNumber(resultSet.getString("PROGRESS_REPORT_NUMBER"));
				awardView.setProgressReportStatus(resultSet.getString("PROGRESS_REPORT_STATUS"));
				awardView.setUpdateTimeStamp(resultSet.getDate("PR_UPDATE_TIMESTAMP") != null ? new Timestamp(resultSet.getDate("PR_UPDATE_TIMESTAMP").getTime()) : null);
				awardView.setCreateUserName(resultSet.getString("CREATE_USERNAME"));
				awardView.setUpdateUserName(resultSet.getString("UPDATE_USERNAME"));
				awardView.setAwardStartDate(resultSet.getDate("AWARD_START_DATE") != null ? new Date(resultSet.getDate("AWARD_START_DATE").getTime()) : null);
				awardView.setAwardEndDate(resultSet.getDate("AWARD_END_DATE") != null ? new Date(resultSet.getDate("AWARD_END_DATE").getTime()) : null);
				awardView.setReportTrackingId(resultSet.getString("AWARD_REPORT_TRACKING_ID"));
				awardView.setReportClassCode(resultSet.getString("REPORT_CLASS_CODE"));
				awardView.setReportClassDescription(resultSet.getString("REPORT_CLASS_DESCRIPTION"));
				awardView.setDueDate(resultSet.getDate("DUE_DATE") != null ? new Date(resultSet.getDate("DUE_DATE").getTime()) : null);
				awardView.setSubmittedDate(resultSet.getDate("WORKFLOW_START_DATE") != null ? new Date(resultSet.getDate("WORKFLOW_START_DATE").getTime()) : null);
				awardView.setReportType(resultSet.getString("REPORT_TYPE"));
				awardViews.add(awardView);
			}
			dashBoardProfile.setProgressReportViews(awardViews);
			dashBoardProfile.setTotalServiceRequest(getProgressReportDashboardCount(vo));
		} catch (SQLException e) {
			logger.error("Error in getProgressReportDashboard {}", e.getMessage());
		}
		return dashBoardProfile;
	}

	private Integer getProgressReportDashboardCount(ProgressReportDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer count = 0;
		String property1 = vo.getProperty1();
		String property2 = vo.getProperty2();
		String property3 = vo.getProperty3();
		String property4 = vo.getProperty4();
		String property5 = vo.getProperty5();
		String personId = vo.getPersonId();
		String dueDateString = vo.getProperty13();
		String tabName = vo.getTabName();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_PROGRESS_REPORT_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, property1);
				statement.setString(2, property2);
				statement.setString(3, property3);
				statement.setString(4, property4);
				statement.setString(5, personId);
				statement.setString(6, dueDateString);
				statement.setString(7, setProgressReportSortOrder(sort));
				statement.setInt(8, 0);
				statement.setInt(9, 0);
				statement.setString(10, tabName);
				statement.setBoolean(11, true);
				statement.setString(12, isAdvancedSearch);
				statement.setString(13, property5);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_PROGRESS_REPORT_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, property1);
				statement.setString(3, property2);
				statement.setString(4, property3);
				statement.setString(5, property4);
				statement.setString(6, personId);
				statement.setString(7, dueDateString);
				statement.setString(8, setProgressReportSortOrder(sort));
				statement.setInt(9, 0);
				statement.setInt(10, 0);
				statement.setString(11, tabName);
				statement.setBoolean(12, true);
				statement.setString(13, isAdvancedSearch);
				statement.setString(14, property5);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.error("Error in getProgressReportDashboardCount {}", e.getMessage());
		}
		return count;
	}
	
	private String setProgressReportSortOrder(Map<String, String> sort) {
		String sortOrder = null;
		if (!sort.isEmpty()) {
			for (Map.Entry<String, String> mapElement : sort.entrySet()) {
				if (mapElement.getKey().equals("accountNumber")) {
					sortOrder = (sortOrder == null ? "T.ACCOUNT_NUMBER " + mapElement.getValue() : sortOrder + ", T.ACCOUNT_NUMBER " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("awardNumber")) {
					sortOrder = (sortOrder == null ? "T.AWARD_NUMBER " + mapElement.getValue() : sortOrder + ", T.AWARD_NUMBER " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("awardPersons.fullName")) {
					sortOrder = (sortOrder == null ? "T.FULL_NAME " + mapElement.getValue() : sortOrder + ", T.FULL_NAME " + mapElement.getValue());
				}		
				if (mapElement.getKey().equals("leadUnit.unitName")) {
					sortOrder = (sortOrder == null ? "T.UNIT_NAME " + mapElement.getValue() : sortOrder + ", T.UNIT_NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("sponsor.sponsorName")) {
					sortOrder = (sortOrder == null ? "SPONSOR_NAME " + mapElement.getValue() : sortOrder + ", SPONSOR_NAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("title")) {
					sortOrder = (sortOrder == null ? "T.TITLE " + mapElement.getValue() : sortOrder + ", T.TITLE " + mapElement.getValue());
				}				
				if (mapElement.getKey().equals("awardSequenceStatus")) {
					sortOrder = (sortOrder == null ? "T.AWARD_SEQUENCE_STATUS " + mapElement.getValue() : sortOrder + ", T.AWARD_SEQUENCE_STATUS " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("sponsorAwardNumber")) {
					sortOrder = (sortOrder == null ? "T.SPONSOR_AWARD_NUMBER " + mapElement.getValue() : sortOrder + ", T.SPONSOR_AWARD_NUMBER " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("progressReportNumber")) {
					sortOrder = (sortOrder == null ? "T.PROGRESS_REPORT_NUMBER " + mapElement.getValue() : sortOrder + ", T.PROGRESS_REPORT_NUMBER " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("createUserName")) {
					sortOrder = (sortOrder == null ? "T.CREATE_USERNAME " + mapElement.getValue() : sortOrder + ", T.CREATE_USERNAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("updateUserName")) {
					sortOrder = (sortOrder == null ? "T.UPDATE_USERNAME " + mapElement.getValue() : sortOrder + ", T.UPDATE_USERNAME " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("progressReportStatus")) {
					sortOrder = (sortOrder == null ? "T.PROGRESS_REPORT_STATUS " + mapElement.getValue() : sortOrder + ", T.PROGRESS_REPORT_STATUS " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("reportClassDescription")) {
					sortOrder = (sortOrder == null ? "T.REPORT_CLASS_DESCRIPTION " + mapElement.getValue() : sortOrder + ", T.REPORT_CLASS_DESCRIPTION " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("dueDate")) {
					sortOrder = (sortOrder == null ? "T.DUE_DATE " + mapElement.getValue() : sortOrder + ", T.DUE_DATE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("submittedDate")) {
					sortOrder = (sortOrder == null ? "T.WORKFLOW_START_DATE " + mapElement.getValue() : sortOrder + ", T.WORKFLOW_START_DATE " + mapElement.getValue());
				}
				if (mapElement.getKey().equals("reportType")) {
					sortOrder = (sortOrder == null ? "T.REPORT_TYPE " + mapElement.getValue() : sortOrder + ", T.REPORT_TYPE " + mapElement.getValue());
				}
			}
		}
		return sortOrder;
	}

	@Override
	public DashBoardProfile getDashBoardDataForAgreementBasedOnCategory(AgreementDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<AgreementHeader> agreementHeaders = new ArrayList<>();
		String requestorPersonId = vo.getProperty1();
		String categoryCode = vo.getProperty2();
		String tabType = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String sortBy = vo.getSortBy();
		String reverse = vo.getReverse();
		Boolean isDownload = vo.getIsDownload();
		try {
			if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
				statement = connection.prepareCall("{call GET_AGMENT_DASHBOARD_CATEGORY(?,?,?,?,?,?,?)}");
				statement.setString(1, requestorPersonId);
				statement.setString(2, categoryCode);
				statement.setString(3, sortAgreement(sortBy,reverse));
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(5, (pageNumber == null ? 0 : pageNumber));
				statement.setString(6, tabType);
				statement.setBoolean(7, isDownload);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
				String procedureName = "GET_AGMENT_DASHBOARD_CATEGORY";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setString(1, requestorPersonId);
				statement.setString(2, categoryCode);
				statement.setString(3, sortAgreement(sortBy,reverse));
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(5, (pageNumber == null ? 0 : pageNumber));
				statement.setString(6, tabType);
				statement.setString(7, isDownload.toString().toUpperCase());
				statement.registerOutParameter(8, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(8);
			}
			while (resultSet.next()) {
				AgreementHeader agreementHeader = new AgreementHeader();
				if (resultSet.getString("TITLE") != null) {
					agreementHeader.setTitle((resultSet.getString("TITLE")));
				}
				agreementHeader.setAgreementRequestId((resultSet.getInt("AGREEMENT_REQUEST_ID")));
				if (resultSet.getDate("START_DATE") != null) {
					agreementHeader.setStartDate((resultSet.getTimestamp("START_DATE")));
				}
				if (resultSet.getDate("UPDATE_TIMESTAMP") != null) {
					agreementHeader.setUpdateTimestamp((resultSet.getTimestamp("UPDATE_TIMESTAMP")));
				}
				if (resultSet.getString("TYPE") != null) {
					agreementHeader.setAgreementTypeCode((resultSet.getString("TYPE")));
				}
				if (resultSet.getDate("END_DATE") != null) {
					agreementHeader.setEndDate((resultSet.getTimestamp("END_DATE")));
				}
				if (resultSet.getString("CATEGORY_NAME") != null) {
					agreementHeader.setCategoryCode((resultSet.getString("CATEGORY_NAME")));
				}
				if (resultSet.getString("STATUS") != null) {
					agreementHeader.setAgreementStatusCode(resultSet.getString("STATUS"));
				}
				if (resultSet.getString("UNIT") != null) {
					agreementHeader.setUnitNumber(resultSet.getString("UNIT"));
				}
				if (resultSet.getString("CONTRACT_VALUE") != null) {
					BigDecimal contractValue=new BigDecimal(resultSet.getString("CONTRACT_VALUE"));
					agreementHeader.setContractValue(contractValue);
				}
				if (resultSet.getString("CURRENCY_CODE") != null) {
					agreementHeader.setCurrencyCode(resultSet.getString("CURRENCY_CODE"));
				}
				if (resultSet.getString("ORGANIZATION") != null) {
					agreementHeader.setOrganization((resultSet.getString("ORGANIZATION")));
				}
				if (resultSet.getString("NEGO_FULL_NAME") != null) {
					agreementHeader.setNegotiatorFullName(resultSet.getString("NEGO_FULL_NAME"));
				}
				if (resultSet.getString("PI_FULL_NAME") != null) {
					agreementHeader.setPiFullName(resultSet.getString("PI_FULL_NAME"));
				}
				if (resultSet.getString("CAT_FULL_NAME") != null) {
					agreementHeader.setCatFullName(resultSet.getString("CAT_FULL_NAME"));
				}
				if (resultSet.getString("STO_FULL_NAME") != null) {
					agreementHeader.setStoFullName(resultSet.getString("STO_FULL_NAME"));
				}
				if (resultSet.getString("CA_FULL_NAME") != null) {
					agreementHeader.setCaFullName((resultSet.getString("CA_FULL_NAME")));
				}
				if (resultSet.getString("ADMIN_PERSON_NAME") != null) {
					agreementHeader.setAdminName((resultSet.getString("ADMIN_PERSON_NAME")));
				}
				if (resultSet.getString("REQUESTOR_NAME") != null) {
					agreementHeader.setRequestorName((resultSet.getString("REQUESTOR_NAME")));
				}
				agreementHeaders.add(agreementHeader);
			}
			dashBoardProfile.setAgreementHeaderList(agreementHeaders);
			dashBoardProfile.setInProgressCount(getDashboardAgreementCategoryCount(vo));
		} catch (SQLException e) {
			logger.info("Exception in dashboard Agreement {}: ", e.getMessage());
		}
		return dashBoardProfile;	
	}
	private Integer getDashboardAgreementCategoryCount(AgreementDashboardVO vo) {
		Integer count = 0;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		String requestorPersonId = vo.getProperty1();
		String categoryCode = vo.getProperty2();
		String tabType = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String sortBy = vo.getSortBy();
		String reverse = vo.getReverse();
		Boolean isDownload = vo.getIsDownload();
		try {
			if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
				statement = connection.prepareCall("{call GET_AGMNT_CAT_DASHBOARD_COUNT(?,?,?,?,?,?,?)}");
				statement.setString(1, requestorPersonId);
				statement.setString(2, categoryCode);
				statement.setString(3, sortAgreement(sortBy,reverse));
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(5, (pageNumber == null ? 0 : pageNumber));
				statement.setString(6, tabType);
				statement.setBoolean(7, isDownload);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
				String procedureName = "GET_AGMNT_CAT_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setString(1, requestorPersonId);
				statement.setString(2, categoryCode);
				statement.setString(3, sortAgreement(sortBy,reverse));
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(5, (pageNumber == null ? 0 : pageNumber));
				statement.setString(6, tabType);
				statement.setString(7, isDownload.toString().toUpperCase());
				statement.registerOutParameter(8, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(8);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.info("Exception in dashboard Agreement count : {}", e.getMessage());
		}
		return count;
	}

	@Override
	public DashBoardProfile getCOIDashboard(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<DisclosureView> disclosureViews = new ArrayList<>();
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		String personId = vo.getPersonId();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_COI_DISCLOSURE_DASHBOARD(?,?,?,?,?,?,?)}");
				statement.setString(1, personId);
				statement.setString(2, setCOISortOrder(sort));
				statement.setInt(3, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(4, (pageNumber == null ? 0 : pageNumber));
				statement.setString(5, tabName);
				statement.setBoolean(6, isDownload);
				statement.setString(7, isAdvancedSearch);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_COI_DISCLOSURE_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, personId);
				statement.setString(3, setCOISortOrder(sort));
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(5, (pageNumber == null ? 0 : pageNumber));
				statement.setString(6, tabName);
				statement.setBoolean(7, isDownload);
				statement.setString(8, isAdvancedSearch);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				DisclosureView disclosureView =  new DisclosureView();
				disclosureView.setCoiDisclosureId(resultSet.getInt("DISCLOSURE_ID"));
				disclosureView.setCoiDisclosureNumber(resultSet.getString("DISCLOSURE_NUMBER"));
				disclosureView.setDisclosureStatusCode(Integer.parseInt(resultSet.getString("DISCLOSURE_STATUS_CODE")));
				disclosureView.setDisclosureStatus(resultSet.getString("DISCLOSURE_STATUS"));
				disclosureView.setDisclosureDispositionCode(Integer.parseInt(resultSet.getString("DISPOSITION_STATUS_TYPE_CODE")));
				disclosureView.setDispositionStatus(resultSet.getString("DISPOSITION_STATUS"));
				disclosureView.setSubmittedDate(resultSet.getTimestamp("CERTIFICATION_TIMESTAMP"));
				disclosureView.setReviewStatusTypeCode(resultSet.getString("REVIEW_STATUS_TYPE_CODE"));
				disclosureView.setDisclosureCategoryTypeCode(resultSet.getString("DISCLOSURE_CATEGORY_TYPE_CODE"));
				disclosureView.setDisclosureCategoryType(resultSet.getString("DISCLOSURE_CATEGORY_TYPE"));
				disclosureView.setReviewStatus(resultSet.getString("REVIEW_STATUS"));
				disclosureView.setLastApprovedVersion(resultSet.getInt("DISCLOSURE_VERSION_NUMBER"));
				disclosureView.setDisclosureSequenceStatusCode(resultSet.getString("DISCLOSURE_SEQUENCE_STATUS_CODE"));
				disclosureView.setDisclosureSequenceStatus(resultSet.getString("DISCLOSURE_SEQUENCE_STATUS"));
				disclosureView.setExpirationDate(resultSet.getTimestamp("EXPIRATION_DATE"));
				if ("CURRENT_DISCLOSURES".equals(tabName)) {
					disclosureView.setNoOfProposalInPending(resultSet.getInt("NO_OF_PENDING_PROPOSALS"));
					disclosureView.setNoOfAwardInPending(resultSet.getInt("NO_OF_PENDING_AWARDS"));
					disclosureView.setNoOfProposalInActive(resultSet.getInt("NO_OF_ACTIVE_PROPOSAL"));
					disclosureView.setNoOfAwardInActive(resultSet.getInt("NO_OF_ACTIVE_AWARD"));
				} else if ("PROPOSAL_DISCLOSURES".equals(tabName)) {
					disclosureView.setProposalId(resultSet.getString("PROPOSAL_ID"));
					disclosureView.setProposalTitle(resultSet.getString("TITLE"));
				}
				disclosureView.setNoOfSfiInActive(resultSet.getInt("NO_OF_SFI_IN_ACTIVE"));
				disclosureView.setNoOfSfiInPending(resultSet.getInt("NO_OF_SFI_IN_PENDING"));
				disclosureView.setCreateTimestamp(resultSet.getTimestamp("CREATE_TIMESTAMP"));
				disclosureView.setUpdateTimeStamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				disclosureView.setUpdateUser(resultSet.getString("UPDATE_USER"));
				disclosureView.setPersonId(resultSet.getString("PERSON_ID"));
				disclosureViews.add(disclosureView);
			}
			dashBoardProfile.setDisclosureViews(disclosureViews);
			dashBoardProfile.setTotalServiceRequest(getCOIDashboardCount(vo));
		} catch (SQLException e) {
			logger.error("Error in getCOIDashboard {}", e.getMessage());
		}
		return dashBoardProfile;
	}

	private Integer getCOIDashboardCount(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer count = 0;
		String personId = vo.getPersonId();
		String tabName = vo.getTabName();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_COI_DISCLOSURE_DASHBOARD_COUNT(?,?,?,?,?,?,?)}");
				statement.setString(1, personId);
				statement.setString(2, setCOISortOrder(sort));
				statement.setInt(3, 0);
				statement.setInt(4, 0);
				statement.setString(5, tabName);
				statement.setBoolean(6, true);
				statement.setString(7, isAdvancedSearch);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call GET_COI_DISCLOSURE_DASHBOARD_COUNT (?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, personId);
				statement.setString(3, setCOISortOrder(sort));
				statement.setInt(4, 0);
				statement.setInt(5, 0);
				statement.setString(6, tabName);
				statement.setBoolean(7, true);
				statement.setString(8, isAdvancedSearch);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.error("Error in getCOIDashboardCount {}", e.getMessage());
		}
		return count;
	}
	private String setCOISortOrder(Map<String, String> sort) {
		String sortOrder = null;
		if (!sort.isEmpty()) {
			for (Map.Entry<String, String> mapElement : sort.entrySet()) {
				if (mapElement.getKey().equals("createTimestamp")) {
					sortOrder = (sortOrder == null ? "T.CREATE_TIMESTAMP " + mapElement.getValue() : sortOrder + ", T.CREATE_TIMESTAMP " + mapElement.getValue());
				}
			}
		}
		return sortOrder;
	}

	@Override
	public DashBoardProfile getCOIAdminDashboard(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<DisclosureView> disclosureViews = new ArrayList<>();
		String disclosureId = vo.getProperty1();
		String disclosurePersonId = vo.getProperty2();
		String homeUnit = vo.getProperty3();
		List<String> disclosureStatusCodes = vo.getProperty4();
		List<String> disclosureCategoryTypeCodes = vo.getProperty5();
		String startDate = vo.getProperty6();
		String endDate = vo.getProperty7();
		String entityName = vo.getProperty8();
		String entityCountry = vo.getProperty9();
		String proposalId = vo.getProperty10();
		String title = vo.getProperty11();
		String awardId = vo.getProperty12();
		String projectTypeCode = vo.getProperty14();
		String hasSFIFlag = vo.getProperty15() != null ? (vo.getProperty15().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		String personId = AuthenticatedUser.getLoginPersonId();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_COI_DISCLOSURE_ADMIN_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, disclosureId);
				statement.setString(2, disclosurePersonId);
				statement.setString(3, homeUnit);
				statement.setString(4, disclosureStatusCodes != null && !disclosureStatusCodes.isEmpty() ? String.join(",", disclosureStatusCodes) : null);
				statement.setString(5, disclosureCategoryTypeCodes != null && !disclosureCategoryTypeCodes.isEmpty() ? String.join(",", disclosureCategoryTypeCodes) : null);
				statement.setString(6, startDate);
				statement.setString(7, endDate);
				statement.setString(8, entityName);
				statement.setString(9, entityCountry);
				statement.setString(10, proposalId);
				statement.setString(11, title);
				statement.setString(12, awardId);
				statement.setString(13, personId);
				statement.setString(14, setCOISortOrder(sort));
				statement.setInt(15, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(16, (pageNumber == null ? 0 : pageNumber));
				statement.setString(17, tabName);
				statement.setBoolean(18, isDownload);
				statement.setString(19, isAdvancedSearch);
				statement.setString(20, projectTypeCode);
				statement.setString(21, hasSFIFlag);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_COI_DISCLOSURE_ADMIN_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, disclosureId);
				statement.setString(3, disclosurePersonId);
				statement.setString(4, homeUnit);
				statement.setString(5, disclosureStatusCodes != null && !disclosureStatusCodes.isEmpty() ? String.join(",", disclosureStatusCodes) : null);
				statement.setString(6, disclosureCategoryTypeCodes != null && !disclosureCategoryTypeCodes.isEmpty() ? String.join(",", disclosureCategoryTypeCodes) : null);
				statement.setString(7, startDate);
				statement.setString(8, endDate);
				statement.setString(9, entityName);
				statement.setString(10, entityCountry);
				statement.setString(11, proposalId);
				statement.setString(12, title);
				statement.setString(13, awardId);
				statement.setString(14, personId);
				statement.setString(15, setCOISortOrder(sort));
				statement.setInt(16, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(17, (pageNumber == null ? 0 : pageNumber));
				statement.setString(18, tabName);
				statement.setBoolean(19, isDownload);
				statement.setString(20, isAdvancedSearch);
				statement.setString(21, projectTypeCode);
				statement.setString(22, hasSFIFlag);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				DisclosureView disclosureView =  new DisclosureView();
				disclosureView.setCoiDisclosureId(resultSet.getInt("DISCLOSURE_ID"));
				disclosureView.setCoiDisclosureNumber(resultSet.getString("DISCLOSURE_NUMBER"));
				disclosureView.setDisclosurePersonFullName(resultSet.getString("DISCLOSURE_PERSON_NAME"));
				disclosureView.setDisclosureStatusCode(Integer.parseInt(resultSet.getString("DISCLOSURE_STATUS_CODE")));
				disclosureView.setDisclosureStatus(resultSet.getString("DISCLOSURE_STATUS"));
				disclosureView.setDisclosureDispositionCode(Integer.parseInt(resultSet.getString("DISPOSITION_STATUS_TYPE_CODE")));
				disclosureView.setDispositionStatus(resultSet.getString("DISPOSITION_STATUS"));
				disclosureView.setDisclosureCategoryTypeCode(resultSet.getString("DISCLOSURE_CATEGORY_TYPE_CODE"));
				disclosureView.setDisclosureCategoryType(resultSet.getString("DISCLOSURE_CATEGORY_TYPE"));
				disclosureView.setReviewStatusTypeCode(resultSet.getString("REVIEW_STATUS_TYPE_CODE"));
				disclosureView.setReviewStatus(resultSet.getString("REVIEW_STATUS"));
				disclosureView.setLastApprovedVersion(resultSet.getInt("LAST_APPROVED_VERSION"));
				disclosureView.setLastApprovedVersionDate(resultSet.getTimestamp("LAST_APPROVED_DATE"));
				disclosureView.setDisclosureSequenceStatusCode(resultSet.getString("DISCLOSURE_SEQUENCE_STATUS_CODE"));
				disclosureView.setDisclosureSequenceStatus(resultSet.getString("DISCLOSURE_SEQUENCE_STATUS"));
				disclosureView.setNoOfProposalInPending(resultSet.getInt("NO_OF_PENDING_PROPOSALS"));
				disclosureView.setNoOfAwardInPending(resultSet.getInt("NO_OF_PENDING_AWARDS"));
				disclosureView.setNoOfProposalInActive(resultSet.getInt("NO_OF_ACTIVE_PROPOSAL"));
				disclosureView.setNoOfAwardInActive(resultSet.getInt("NO_OF_ACTIVE_AWARD"));
				disclosureView.setNoOfSfiInActive(resultSet.getInt("NO_OF_SFI_IN_ACTIVE"));
				disclosureView.setNoOfSfiInPending(resultSet.getInt("NO_OF_SFI_IN_PENDING"));
				disclosureView.setUpdateTimeStamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				disclosureView.setUpdateUser(resultSet.getString("UPDATE_USER_FULL_NAME"));
				disclosureView.setReviseComment(resultSet.getString("REVISE_COMMENT"));
				disclosureView.setPersonId(resultSet.getString("PERSON_ID"));
				if (tabName.equals("PENDING_DISCLOSURES")) {
					disclosureView.setReviewId(resultSet.getInt("COI_REVIEW_ID"));
					disclosureView.setReviewDescription(resultSet.getString("REVIEW_DESCRIPTION"));
					disclosureView.setReviewerStatusCode(resultSet.getString("REVIEWER_STATUS_CODE"));
					disclosureView.setReviewerStatus(resultSet.getString("REVIEWER_STATUS"));
					disclosureView.setReviewerFullName(resultSet.getString("REVIEWER_NAME"));
				}
				disclosureViews.add(disclosureView);
			}
			dashBoardProfile.setDisclosureViews(disclosureViews);
			dashBoardProfile.setDisclosureCount(getCOIAdminDashboardCount(vo));
		} catch (Exception e) {
			logger.error("Error in getCOIAdminDashboard {}", e.getMessage());
		}
		return dashBoardProfile;
	}

	private Integer getCOIAdminDashboardCount(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer count = 0;
		String disclosureId = vo.getProperty1();
		String disclosurePersonId = vo.getProperty2();
		String homeUnit = vo.getProperty3();
		List<String> disclosureStatusCodes = vo.getProperty4();
		List<String> disclosureCategoryTypeCodes = vo.getProperty5();
		String startDate = vo.getProperty6();
		String endDate = vo.getProperty7();
		String entityName = vo.getProperty8();
		String entityCountry = vo.getProperty9();
		String proposalId = vo.getProperty10();
		String title = vo.getProperty11();
		String awardId = vo.getProperty12();
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		String projectTypeCode = vo.getProperty14();
		String hasSFIFlag = vo.getProperty15() != null ? (vo.getProperty15().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		Map<String, String> sort = vo.getSort();
		String personId = AuthenticatedUser.getLoginPersonId();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_COI_DISCLOSURE_ADMIN_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, disclosureId);
				statement.setString(2, disclosurePersonId);
				statement.setString(3, homeUnit);
				statement.setString(4, disclosureStatusCodes != null && !disclosureStatusCodes.isEmpty() ? String.join(",", disclosureStatusCodes) : null);
				statement.setString(5, disclosureCategoryTypeCodes != null && !disclosureCategoryTypeCodes.isEmpty() ? String.join(",", disclosureCategoryTypeCodes) : null);
				statement.setString(6, startDate);
				statement.setString(7, endDate);
				statement.setString(8, entityName);
				statement.setString(9, entityCountry);
				statement.setString(10, proposalId);
				statement.setString(11, title);
				statement.setString(12, awardId);
				statement.setString(13, personId);
				statement.setString(14, setCOISortOrder(sort));
				statement.setInt(15, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(16, (pageNumber == null ? 0 : pageNumber));
				statement.setString(17, tabName);
				statement.setBoolean(18, true);
				statement.setString(19, isAdvancedSearch);
				statement.setString(20, projectTypeCode);
				statement.setString(21, hasSFIFlag);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_COI_DISCLOSURE_ADMIN_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, disclosureId);
				statement.setString(3, disclosurePersonId);
				statement.setString(4, homeUnit);
				statement.setString(5, disclosureStatusCodes != null && !disclosureStatusCodes.isEmpty() ? String.join(",", disclosureStatusCodes) : null);
				statement.setString(6, disclosureCategoryTypeCodes != null && !disclosureCategoryTypeCodes.isEmpty() ? String.join(",", disclosureCategoryTypeCodes) : null);
				statement.setString(7, startDate);
				statement.setString(8, endDate);
				statement.setString(9, entityName);
				statement.setString(10, entityCountry);
				statement.setString(11, proposalId);
				statement.setString(12, title);
				statement.setString(13, awardId);
				statement.setString(14, personId);
				statement.setString(15, setCOISortOrder(sort));
				statement.setInt(16, 0);
				statement.setInt(17, 0);
				statement.setString(18, tabName);
				statement.setBoolean(19, true);
				statement.setString(20, isAdvancedSearch);
				statement.setString(21, projectTypeCode);
				statement.setString(22, hasSFIFlag);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (Exception e) {
			logger.error("Error in getCOIAdminDashboardCount {}", e.getMessage());
		}
		return count;
	}

	@Override
	public DashBoardProfile getSFIDashboard(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<COIFinancialEntityDto> coiFinancialEntityDtos = new ArrayList<>();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		String entityName = vo.getProperty8();
		String entityId = vo.getProperty16();
		String hasDisclosureFlag = vo.getProperty17() != null ? (vo.getProperty17().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String hasProposalFlag = vo.getProperty18() != null ? (vo.getProperty18().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String hasAwardFlag = vo.getProperty19() != null ? (vo.getProperty19().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		Map<String, String> sort = vo.getSort();
		String personId = vo.getPersonId();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_SFI_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, personId);
				statement.setString(2, null);
				statement.setInt(3, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(4, (pageNumber == null ? 0 : pageNumber));
				statement.setBoolean(5, isDownload);
				statement.setString(6, isAdvancedSearch);
				statement.setString(7, entityName);
				statement.setString(8, entityId);
				statement.setString(9, hasDisclosureFlag);
				statement.setString(10, hasProposalFlag);
				statement.setString(11, hasAwardFlag);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_SFI_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, personId);
				statement.setString(3, null);
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(5, (pageNumber == null ? 0 : pageNumber));
				statement.setBoolean(6, isDownload);
				statement.setString(7, isAdvancedSearch);
				statement.setString(8, entityName);
				statement.setString(9, entityId);
				statement.setString(10, hasDisclosureFlag);
				statement.setString(11, hasProposalFlag);
				statement.setString(12, hasAwardFlag);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				COIFinancialEntityDto coiFinancialEntityDto = new COIFinancialEntityDto();
				coiFinancialEntityDto.setCoiFinancialEntityId(resultSet.getInt("COI_FINANCIAL_ENTITY_ID"));
				coiFinancialEntityDto.setEntityVersionNumber(resultSet.getInt("ENTITY_VERSION_NUMBER"));
				coiFinancialEntityDto.setCoiEntityName(resultSet.getString("COI_ENTITY_NAME"));
				coiFinancialEntityDto.setInvolvementStartDate(resultSet.getDate("INVOLVEMENT_START_DATE"));
				coiFinancialEntityDto.setCreateTimestamp(resultSet.getTimestamp("CREATE_TIMESTAMP"));
				coiFinancialEntityDto.setLastUpdatedOn(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				coiFinancialEntityDto.setNoOfDisclosures(resultSet.getInt("NO_OF_DISCLOSURES"));
				coiFinancialEntityDto.setIsActive(resultSet.getString("IS_ACTIVE"));
				coiFinancialEntityDto.setNoOfProposals(resultSet.getInt("NO_OF_PROPOSALS"));
				coiFinancialEntityDto.setNoOfAwards(resultSet.getInt("NO_OF_AWARDS"));
				coiFinancialEntityDto.setInvolvementEndDate(resultSet.getDate("INVOLVEMENT_END_DATE"));
				coiFinancialEntityDto.setCoiEntityType(resultSet.getString("ENTITY_TYPE"));
				coiFinancialEntityDto.setCoiEntityCountry(resultSet.getString("COUNTRY_NAME"));
				coiFinancialEntityDto.setCoiEntityEmail(resultSet.getString("EMAIL_ADDRESS"));
				coiFinancialEntityDtos.add(coiFinancialEntityDto);
			}
			dashBoardProfile.setCoiFinancialEntityList(coiFinancialEntityDtos);
			dashBoardProfile.setCoiFinancialEntityListCount(getSFIDashboardCount(vo));
		} catch (SQLException e) {
			logger.error("Error in getSFIDashboard {}", e.getMessage());
		}
		return dashBoardProfile;
	}

	private Integer getSFIDashboardCount(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer count = 0;
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		String entityName = vo.getProperty8();
		String entityId = vo.getProperty16();
		String hasDisclosureFlag = vo.getProperty17() != null ? (vo.getProperty17().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String hasProposalFlag = vo.getProperty18() != null ? (vo.getProperty18().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String hasAwardFlag = vo.getProperty19() != null ? (vo.getProperty19().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String personId = vo.getPersonId();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_SFI_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, personId);
				statement.setString(2, null);
				statement.setInt(3, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(4, (pageNumber == null ? 0 : pageNumber));
				statement.setBoolean(5, isDownload);
				statement.setString(6, isAdvancedSearch);
				statement.setString(7, entityName);
				statement.setString(8, entityId);
				statement.setString(9, hasDisclosureFlag);
				statement.setString(10, hasProposalFlag);
				statement.setString(11, hasAwardFlag);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_SFI_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, personId);
				statement.setString(3, null);
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(5, (pageNumber == null ? 0 : pageNumber));
				statement.setBoolean(6, Boolean.TRUE);
				statement.setString(7, isAdvancedSearch);
				statement.setString(8, entityName);
				statement.setString(9, entityId);
				statement.setString(10, hasDisclosureFlag);
				statement.setString(11, hasProposalFlag);
				statement.setString(12, hasAwardFlag);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.error("Error in getSFIDashboardCount {}", e.getMessage());
		}
		return count;
	}

}
