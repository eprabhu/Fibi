package com.polus.fibicomp.opa.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import com.polus.core.applicationexception.dto.ApplicationException;
import com.polus.core.common.dao.CommonDao;
import com.polus.core.person.dao.PersonDao;
import com.polus.fibicomp.opa.pojo.OPAReviewStatusType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.opa.dto.OPAAssignAdminDto;
import com.polus.fibicomp.opa.dto.OPACommonDto;
import com.polus.fibicomp.opa.dto.OPADashboardDto;
import com.polus.fibicomp.opa.dto.OPADashboardRequestDto;
import com.polus.fibicomp.opa.dto.OPADashboardResponseDto;
import com.polus.fibicomp.opa.dto.OPASubmitDto;
import com.polus.fibicomp.opa.pojo.OPADisclosure;
import com.polus.fibicomp.opa.pojo.OPADisclosureStatusType;
import com.polus.fibicomp.opa.pojo.OPAFormBuilderDetails;
import com.polus.fibicomp.opa.pojo.OPAPersonType;
import com.polus.core.security.AuthenticatedUser;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "opaDaoImpl")
public class OPADaoImpl implements OPADao {

    @Autowired
    private HibernateTemplate hibernateTemplate;

    @Autowired
    private CommonDao commonDao;

    @Autowired
	private PersonDao personDao;

  	protected static Logger logger = LogManager.getLogger(OPADaoImpl.class.getName());
  	private static final String DESIGNATION_STATUS_CODE_FACULTY = "1";
  	private static final String DESIGNATION_STATUS_FACULTY = "\"Y\"";
  	private static final String DESIGNATION_STATUS_STAFF = "\"N\"";

    @Override
    public boolean canCreateOpaDisclosure(String personId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement = null;
        try {
            String functionName = "FN_OPA_DISCLOSURE_REQUIRED";
            String functionCall = "{ ? = call " + functionName + "(?) }";
            statement = connection.prepareCall(functionCall);
            statement.registerOutParameter(1, OracleTypes.INTEGER);
            statement.setString(2, personId);
            statement.execute();
            int result = statement.getInt(1);
            if (result == 1) {
                return true;
            }
        } catch (SQLException e) {
            throw new ApplicationException(e.getMessage(),e, Constants.DB_PROC_ERROR);
        }
        return false;
    }

    @Override
    public Timestamp submitOPADisclosure(OPASubmitDto opaSubmitDto) {
        Timestamp timesStamp = commonDao.getCurrentTimestamp();
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE OPADisclosure d SET d.certificationText = :certificationText, ");
        hqlQuery.append("d.certifiedBy = :certifiedBy, d.submissionTimestamp = :submissionTimestamp, ");
        hqlQuery.append("d.reviewStatusCode = :reviewStatusCode, d.updateTimestamp = :updateTimestamp,");
        hqlQuery.append("d.updateUser = :updateUser, d.dispositionStatusCode = :dispositionStatusCode ");
        hqlQuery.append("WHERE d.opaDisclosureId = :opaDisclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId", opaSubmitDto.getOpaDisclosureId());
        query.setParameter("certificationText", opaSubmitDto.getCertificationText());
        query.setParameter("certifiedBy", AuthenticatedUser.getLoginPersonId());
        query.setParameter("submissionTimestamp", timesStamp);
		if (opaSubmitDto.getOpaDisclosureStatus() != null) {
			query.setParameter("reviewStatusCode", opaSubmitDto.getOpaDisclosureStatus());
		} else {
			query.setParameter("reviewStatusCode", Constants.OPA_DISCLOSURE_STATUS_SUBMIT);
		}
        query.setParameter("dispositionStatusCode", Constants.OPA_DISPOSITION_STATUS_PENDING);
        query.setParameter("updateTimestamp", timesStamp);
        query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
        query.executeUpdate();
        return timesStamp;
    }

    @Override
    public Timestamp returnOrWithdrawOPADisclosure(String opaStatusCode, Integer opaDisclosureId) {
        Timestamp timesStamp = commonDao.getCurrentTimestamp();
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE OPADisclosure d SET d.certificationText = :certificationText, ");
        hqlQuery.append("d.certifiedBy = :certifiedBy, d.submissionTimestamp = :submissionTimestamp, ");
        hqlQuery.append("d.reviewStatusCode = :reviewStatusCode, d.updateTimestamp = :updateTimestamp, ");
        hqlQuery.append("d.updateUser = :updateUser ");
        hqlQuery.append("WHERE d.opaDisclosureId = :opaDisclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId",opaDisclosureId);
        query.setParameter("certificationText", null);
        query.setParameter("certifiedBy", null);
        query.setParameter("submissionTimestamp", null);
        query.setParameter("reviewStatusCode", opaStatusCode);
        query.setParameter("updateTimestamp", timesStamp);
        query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
        query.executeUpdate();
        return timesStamp;
    }

    @Override
    public Timestamp assignAdminOPADisclosure(OPAAssignAdminDto assignAdminDto) {
        Timestamp timesStamp = commonDao.getCurrentTimestamp();
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE OPADisclosure d SET d.adminGroupId = :adminGroupId, ");
        hqlQuery.append("d.adminPersonId = :adminPersonId, d.updateTimestamp = :updateTimestamp, ");
		hqlQuery.append(assignAdminDto.getOpaDisclosureStatus() != null ? "d.reviewStatusCode = :reviewStatusCode, " : "");
        hqlQuery.append("d.updateUser = :updateUser ");
        hqlQuery.append("WHERE d.opaDisclosureId = :opaDisclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId",assignAdminDto.getOpaDisclosureId());
        query.setParameter("adminGroupId", assignAdminDto.getAdminGroupId());
        query.setParameter("adminPersonId", assignAdminDto.getAdminPersonId());
        if(assignAdminDto.getOpaDisclosureStatus() != null ) {
        	query.setParameter("reviewStatusCode", assignAdminDto.getOpaDisclosureStatus());
        }
        query.setParameter("updateTimestamp", timesStamp);
        query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
        query.executeUpdate();
        return timesStamp;
    }

    @Override
    public Timestamp completeOPADisclosure(Integer opaDisclosureId) {
        Timestamp timesStamp = commonDao.getCurrentTimestamp();
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE OPADisclosure d SET ");
        hqlQuery.append("d.reviewStatusCode = :reviewStatusCode, d.updateTimestamp = :updateTimestamp, ");
        hqlQuery.append("d.updateUser = :updateUser, d.dispositionStatusCode = :dispositionStatusCode ");
        hqlQuery.append("WHERE d.opaDisclosureId = :opaDisclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId",opaDisclosureId);
        query.setParameter("reviewStatusCode", Constants.OPA_DISCLOSURE_STATUS_COMPLETED);
        query.setParameter("dispositionStatusCode", Constants.OPA_DISPOSITION_STATUS_COMPLETED);
        query.setParameter("updateTimestamp", timesStamp);
        query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
        query.executeUpdate();
        return timesStamp;
    }

    @Override
    public boolean isOPAWithStatuses(List<String> opaDisclosureStatuses, String dispositionStatus, Integer opaDisclosureId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT case when (count(d.opaDisclosureId) > 0) then true else false end FROM OPADisclosure d WHERE ");
        if (opaDisclosureStatuses != null && !opaDisclosureStatuses.isEmpty()) {
            hqlQuery.append(" d.reviewStatusCode IN (:reviewStatusCodes) AND ");
        }
        if (dispositionStatus != null)
            hqlQuery.append("d.dispositionStatusCode = :dispositionStatusCode AND ");
        hqlQuery.append("d.opaDisclosureId = :opaDisclosureId ");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId",opaDisclosureId);
        if (opaDisclosureStatuses != null && !opaDisclosureStatuses.isEmpty()) {
            query.setParameter("reviewStatusCodes", opaDisclosureStatuses);
        }
        if (dispositionStatus != null)
            query.setParameter("dispositionStatusCode", dispositionStatus);
        return (boolean) query.getSingleResult();
    }

    @Override
	public OPACommonDto createOpaDisclosure(String personId, String homeUnit) {
		Session session = hibernateTemplate.getSessionFactory().openSession();
		try {
			SessionImpl sessionImpl = (SessionImpl) session;
			Transaction transaction = session.beginTransaction();
			Connection connection = sessionImpl.connection();
			CallableStatement statement = connection.prepareCall("{call INSERT_OPA_DISCLOSURE_DETAILS(?,?,?)}");
			statement.setString(1, personId);
			statement.setString(2, homeUnit);
			statement.setString(3, AuthenticatedUser.getLoginUserName());
			statement.execute();
			transaction.commit();
			statement.getMoreResults();
			try (ResultSet resultSet = statement.getResultSet()) {
				while (resultSet.next()) {
					return OPACommonDto.builder()
							.opaDisclosureId(resultSet.getInt("LI_OPA_DISCLOSURE_ID"))
							.opaDisclosureNumber(resultSet.getString("LS_OPA_DISCLOSURE_NUMBER")).build();
				}
			}
		} catch (Exception e) {
			logger.error("Exception in createOpaDisclosure {}", e.getMessage());
            throw new ApplicationException("Unable to create disclosure", e, Constants.DB_PROC_ERROR);
		} finally {
			session.close();
		}
		return null;
	  }

    @Override
    public boolean isAdminAssigned(Integer opaDisclosureId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT case when (count(d.opaDisclosureId) > 0) then true else false end ");
        hqlQuery.append("FROM OPADisclosure d WHERE d.adminPersonId IS NOT NULL AND  ");
        hqlQuery.append("d.opaDisclosureId = :opaDisclosureId ");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId",opaDisclosureId);
        return (boolean) query.getSingleResult();
    }

	@Override
	public OPADisclosure getOPADisclosure(Integer opaDisclosureId) {
		return hibernateTemplate.get(OPADisclosure.class, opaDisclosureId);
	}

    @Override
    public OPADashboardResponseDto getOPADashboard(OPADashboardRequestDto requestDto) {

        List<OPADashboardDto> opaDashboardDtos = new ArrayList<>();
        try {
            ResultSet rset = getOPADashboardResultSet(requestDto, false);
            while (rset.next()) {
                String reviewers = rset.getString("REVIEWERS");
                List<List<String>> reviewerList = new ArrayList<>();
                if (reviewers != null && !reviewers.isEmpty()) {
                    String[] reviewerArray = reviewers.split(";");
                    Arrays.stream(reviewerArray)
                            .forEach(reviewer -> {
                                List<String> subList = Arrays.stream(reviewer.split(":"))
                                        .map(String::trim)
                                        .collect(Collectors.toList());
                                reviewerList.add(subList);
                            });
                }
                opaDashboardDtos.add(
                        OPADashboardDto.builder()
                        .opaDisclosureId(rset.getInt("OPA_DISCLOSURE_ID"))
                        .opaDisclosureNumber(rset.getString("OPA_DISCLOSURE_NUMBER"))
                        .opaCycleNumber(rset.getInt("OPA_CYCLE_NUMBER"))
                        .periodStartDate(rset.getDate("PERIOD_START_DATE"))
                        .periodEndDate(rset.getDate("PERIOD_END_DATE"))
                        .opaCycleStatus(rset.getBoolean("OPA_CYCLE_STATUS"))
                        .openDate(rset.getDate("OPEN_DATE"))
                        .closeDate(rset.getDate("CLOSE_DATE"))
                        .personName(rset.getString("PERSON_NAME"))
                        .homeUnitName(rset.getString("UNIT_NAME"))
                        .homeUnit(rset.getString("UNIT_NUMBER"))
                        .isFaculty(rset.getBoolean("IS_FACULTY"))
                        .isFallSabatical(rset.getBoolean("IS_FALL_SABATICAL"))
                        .isSpringSabatical(rset.getBoolean("IS_SPRING_SABATICAL"))
                        .receivedSummerComp(rset.getBoolean("RECEIVED_SUMMER_COMP"))
                        .summerCompMonths(rset.getBigDecimal("SUMMER_COMP_MONTHS"))
                        .hasPotentialConflict(rset.getBoolean("HAS_POTENTIAL_CONFLICT"))
                        .conflictDescription(rset.getString("CONFLICT_DESCRIPTION"))
                        .createTimestamp(rset.getTimestamp("CREATE_TIMESTAMP"))
                        .createUser(rset.getString("CREATE_USER"))
                        .submissionTimestamp(rset.getTimestamp("SUBMISSION_TIMESTAMP"))
                        .reviewStatusCode(rset.getString("REVIEW_STATUS_CODE"))
                        .dispositionStatusCode(rset.getString("DISPOSITION_STATUS_CODE"))
                        .dispositionStatus(rset.getString("DISPOSITION_STATUS"))
                        .reviewStatus(rset.getString("OPA_DISCLOSURE_STATUS"))
                        .updateTimeStamp(rset.getTimestamp("UPDATE_TIMESTAMP"))
                        .updateUser(rset.getString("UPDATE_USER"))
                        .updateUserFullName(rset.getString("UPDATE_USER_FULL_NAME"))
                        .adminPersonName(rset.getString("ADMIN_FULL_NAME"))
                        .adminGroupName(rset.getString("ADMIN_GROUP_NAME"))
                        .reviewers(reviewerList)
                        .build()
                );
            }
            Integer count = 0;
            rset = getOPADashboardResultSet(requestDto, true);
            while (rset.next()) {
                count = rset.getInt(1);
            }
            return OPADashboardResponseDto.builder().data(opaDashboardDtos).count(count).build();
        } catch (Exception e) {
            e.printStackTrace();
            logger.error("Exception on getOPADashboard {}", e.getMessage());
            throw new ApplicationException("Unable to fetch opa dashboard details", e, Constants.DB_PROC_ERROR);
        }
    }

    @Override
    public ResultSet getOPADashboardResultSet(OPADashboardRequestDto requestDto, boolean isCount) throws SQLException {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        Integer currentPage = requestDto.getCurrentPage();
        Integer pageNumber = requestDto.getPageNumber();
        List<String> disclosureStatusCodes = requestDto.getReviewStatusCodes();
        List<String> dispositionStatusCodes = requestDto.getDispositionStatusCodes();
		List<String> designationStatusCodes = requestDto.getDesignationStatusCodes();
		designationStatusCodes = designationStatusCodes != null && !designationStatusCodes.isEmpty()
				&& designationStatusCodes.contains("1") && designationStatusCodes.contains("2") ? null
						: designationStatusCodes;
		if (designationStatusCodes != null && !designationStatusCodes.isEmpty()) {
			designationStatusCodes = designationStatusCodes.stream()
					.map(code -> DESIGNATION_STATUS_CODE_FACULTY.equals(code) ? DESIGNATION_STATUS_FACULTY
							: DESIGNATION_STATUS_STAFF)
					.collect(Collectors.toList());
		}
        String submissionTimestamp = requestDto.getSubmissionTimestamp();
        String unitNumber = requestDto.getUnitNumber();
		Boolean fetchAllRecords = requestDto.getFetchAllRecords();
        CallableStatement statement = connection.prepareCall("{call GET_COI_OPA_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
        statement.setString(1, AuthenticatedUser.getLoginPersonId());
        statement.setString(2, requestDto.getFilterType());
        statement.setBoolean(3, isCount);
        statement.setString(4, setOPASortOrder(requestDto.getSort()));
        statement.setInt(5, (currentPage == null ? 0 : currentPage - 1));
        statement.setInt(6, (pageNumber == null ? 0 : pageNumber));
        statement.setString(7, requestDto.getTabType());
        statement.setString(8, disclosureStatusCodes != null &&
                !disclosureStatusCodes.isEmpty() ? String.join(",", disclosureStatusCodes) : null);
        statement.setString(9, dispositionStatusCodes != null &&
                !dispositionStatusCodes.isEmpty() ? String.join(",", dispositionStatusCodes) : null);
        statement.setString(10, submissionTimestamp);
        statement.setString(11, unitNumber);
		statement.setBoolean(12, fetchAllRecords != null && fetchAllRecords);
        statement.setString(13, requestDto.getPersonId());
        statement.setString(14, requestDto.getEntityId() != null ? requestDto.getEntityId().toString() : null);
        statement.setString(15, designationStatusCodes != null &&
                !designationStatusCodes.isEmpty() ? String.join(",", designationStatusCodes) : null);
        statement.setString(16, requestDto.getPeriodStartDate());
        statement.setString(17, requestDto.getPeriodEndDate());
        statement.execute();
        return  statement.getResultSet();
    }

    private String setOPASortOrder(Map<String, String> sort) {
        String sortOrder = null;
        if (!sort.isEmpty()) {
            for (Map.Entry<String, String> mapElement : sort.entrySet()) {
                if (mapElement.getKey().equals("createTimestamp")) {
                    sortOrder = (sortOrder == null ? "T.CREATE_TIMESTAMP " + mapElement.getValue() : sortOrder + ", T.CREATE_TIMESTAMP " + mapElement.getValue());
                } else if (mapElement.getKey().equals("person")) {
                    sortOrder = (sortOrder == null ? "T.PERSON_NAME " + mapElement.getValue() : sortOrder + ", T.PERSON_NAME " + mapElement.getValue());
                } else if (mapElement.getKey().equals("submissionTimestamp")) {
                    sortOrder = (sortOrder == null ? "T.SUBMISSION_TIMESTAMP " + mapElement.getValue() : sortOrder + ", T.SUBMISSION_TIMESTAMP " + mapElement.getValue());
                } else if (mapElement.getKey().equals("updateTimeStamp")) {
                    sortOrder = (sortOrder == null ? "T.UPDATE_TIMESTAMP " + mapElement.getValue() : sortOrder + ", T.UPDATE_TIMESTAMP " + mapElement.getValue());
                } else if (mapElement.getKey().equals("dispositionStatus")) {
                    sortOrder = (sortOrder == null ? "T.DISPOSITION_STATUS " + mapElement.getValue() : sortOrder + ", T.DISPOSITION_STATUS " + mapElement.getValue());
                } else if (mapElement.getKey().equals("disclosureStatus")) {
                    sortOrder = (sortOrder == null ? "T.OPA_DISCLOSURE_STATUS " + mapElement.getValue() : sortOrder + ", T.OPA_DISCLOSURE_STATUS " + mapElement.getValue());
                } else if (mapElement.getKey().equals("homeUnitName")) {
                    sortOrder = (sortOrder == null ? "T.UNIT_NAME " + mapElement.getValue() : sortOrder + ", T.UNIT_NAME " + mapElement.getValue());
                }
            }
        }
        return sortOrder;
    }

    @Override
	public OPAFormBuilderDetails saveOrUpdateOpaFormBuilderDetails(OPAFormBuilderDetails opaFormBuilderDetails) {
		hibernateTemplate.saveOrUpdate(opaFormBuilderDetails);
		return opaFormBuilderDetails;
	}

	@Override
	public List<OPAFormBuilderDetails> getOpaFormBuilderDetailsByOpaDisclosureId(Integer opaDisclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
	    CriteriaBuilder builder = session.getCriteriaBuilder();
	    CriteriaQuery<OPAFormBuilderDetails> query = builder.createQuery(OPAFormBuilderDetails.class);
	    Root<OPAFormBuilderDetails> root = query.from(OPAFormBuilderDetails.class);
	    query.select(root).where(builder.equal(root.get("opaDisclosureId"), opaDisclosureId));
	    return session.createQuery(query).getResultList();
		
	}

	@Override
	public String getAssignedAdmin(Integer opaDisclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
	    CriteriaBuilder builder = session.getCriteriaBuilder();
	    CriteriaQuery<String> query = builder.createQuery(String.class);
	    Root<OPADisclosure> root = query.from(OPADisclosure.class);
	    query.select(root.get("adminPersonId")).where(builder.equal(root.get("opaDisclosureId"), opaDisclosureId));
	    return session.createQuery(query).getSingleResult();
	}

	@Override
	public List<OPADisclosure> getActiveAndPendingOpaDisclosure(String personId) {
		List<OPADisclosure> opaDisclosures = new ArrayList<>();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<OPADisclosure> query = builder.createQuery(OPADisclosure.class);
			Root<OPADisclosure> rootOpaDisclosure = query.from(OPADisclosure.class);
			query.where(builder.and(builder.equal(rootOpaDisclosure.get("personId"), personId),
					builder.equal(rootOpaDisclosure.get("dispositionStatusCode"), Constants.OPA_DISPOSITION_STATUS_COMPLETED)));
			query.orderBy(builder.desc(rootOpaDisclosure.get("updateTimestamp")));
			List<OPADisclosure> opaDisclData = session.createQuery(query).getResultList();
			if (opaDisclData != null && !opaDisclData.isEmpty()) {
				OPADisclosure opaDisclosure = opaDisclData.get(0);
				opaDisclosure.setUpdateUserFullName(personDao.getUserFullNameByUserName(opaDisclosure.getUpdateUser()));
				opaDisclosure.setAdminPersonName(opaDisclosure.getAdminPersonId() != null ? personDao.getPersonFullNameByPersonId(opaDisclosure.getAdminPersonId()) : null);
				opaDisclosure.setAdminGroupName(opaDisclosure.getAdminGroupId() != null ? commonDao.getAdminGroupByGroupId(opaDisclosure.getAdminGroupId()).getAdminGroupName() : null);
				opaDisclosure.setHomeUnitName(commonDao.getUnitName(opaDisclosure.getHomeUnit()));
				opaDisclosures.add(opaDisclosure);
			}
			OPADisclosure opaDisclosure = getPendingOpaDisclosure(personId);
			if (opaDisclosure != null) {
				opaDisclosure.setUpdateUserFullName(personDao.getUserFullNameByUserName(opaDisclosure.getUpdateUser()));
				opaDisclosure.setHomeUnitName(commonDao.getUnitName(opaDisclosure.getHomeUnit()));
				opaDisclosures.add(opaDisclosure);
			}
		} catch (Exception ex) {
			throw new ApplicationException("Unable to fetch Active Disclosure", ex, Constants.JAVA_ERROR);
		}
		return opaDisclosures;
	}

	private OPADisclosure getPendingOpaDisclosure(String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<OPADisclosure> query = builder.createQuery(OPADisclosure.class);
		Root<OPADisclosure> rootOpaDisclosure = query.from(OPADisclosure.class);
		query.where(builder.and(builder.equal(rootOpaDisclosure.get("personId"), personId),
				builder.equal(rootOpaDisclosure.get("dispositionStatusCode"), Constants.OPA_DISPOSITION_STATUS_PENDING)));
		query.orderBy(builder.desc(rootOpaDisclosure.get("updateTimestamp")));
		List<OPADisclosure> opaDisclData = session.createQuery(query).getResultList();
		return !opaDisclData.isEmpty() ? opaDisclData.get(0) : null;
	}

    @Override
    public Timestamp updateOPADisclosureUpDetails(Integer opaDisclosureId, Timestamp timesStamp) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE OPADisclosure d SET ");
        hqlQuery.append("d.updateTimestamp = :updateTimestamp, d.updateUser = :updateUser ");
        hqlQuery.append("WHERE d.opaDisclosureId = :opaDisclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId",opaDisclosureId);
        query.setParameter("updateTimestamp", timesStamp);
        query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
        query.executeUpdate();
        return timesStamp;
    }

    @Override
    public void updateOPADisclosureStatuses(Integer opaDisclosureId, Timestamp updateTimesStamp, String reviewStatusCode, String dispositionStatusCode) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE OPADisclosure d SET ");
        hqlQuery.append("d.updateTimestamp = :updateTimestamp, ");
        hqlQuery.append("d.updateUser = :updateUser ");
        if (reviewStatusCode != null)
            hqlQuery.append(", d.reviewStatusCode = :reviewStatusCode ");
        if (dispositionStatusCode != null)
            hqlQuery.append(", d.dispositionStatusCode = :dispositionStatusCode ");
        hqlQuery.append("WHERE d.opaDisclosureId = :opaDisclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId",opaDisclosureId);
        if (reviewStatusCode != null)
            query.setParameter("reviewStatusCode", reviewStatusCode);
        if (dispositionStatusCode != null)
            query.setParameter("dispositionStatusCode", dispositionStatusCode);
        query.setParameter("updateTimestamp", updateTimesStamp);
        query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
        query.executeUpdate();
    }

    @Override
    public OPAReviewStatusType getOPADisclosureStatusType(String statusTypeCode) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT s FROM  OPAReviewStatusType s ");
        hqlQuery.append("WHERE s.reviewStatusCode = :reviewStatusCode");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("reviewStatusCode", statusTypeCode);
        List<OPAReviewStatusType> resultData = query.getResultList();
        if(resultData != null  && !resultData.isEmpty()) {
            return resultData.get(0);
        }
        return null;
    }

	@Override
	public List<OPAPersonType> getOpaPersonType() {
		return hibernateTemplate.loadAll(OPAPersonType.class);
	}

	@Override
	public boolean isSameAdminPersonOrGroupAdded(Integer adminGroupId, String adminPersonId, Integer opaDisclosureId) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("SELECT case when (count(c.opaDisclosureId) > 0) then true else false end ");
		hqlQuery.append("FROM OPADisclosure c WHERE  c.adminPersonId = :adminPersonId ");
		if (adminGroupId != null)
			hqlQuery.append("AND c.adminGroupId = :adminGroupId ") ;
		hqlQuery.append("AND c.opaDisclosureId = : opaDisclosureId");
		Query query = session.createQuery(hqlQuery.toString());
		if (adminGroupId != null)
			query.setParameter("adminGroupId", adminGroupId);
		query.setParameter("adminPersonId", adminPersonId);
		query.setParameter("opaDisclosureId", opaDisclosureId);
		return (boolean)query.getSingleResult();
	}

	@Override
	public boolean isAdminPersonOrGroupAdded(Integer opaDisclosureId) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("SELECT case when (count(c.opaDisclosureId) > 0) then false else true end ");
		hqlQuery.append("FROM OPADisclosure c WHERE  c.adminPersonId is null AND c.adminGroupId is null ");
		hqlQuery.append("AND c.opaDisclosureId = : opaDisclosureId");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("opaDisclosureId", opaDisclosureId);
		return (boolean)query.getSingleResult();
	}

}
