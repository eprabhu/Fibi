package com.polus.fibicomp.opa.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.persistence.Query;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.security.AuthenticatedUser;
import oracle.jdbc.OracleTypes;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


import com.polus.fibicomp.opa.dto.OPAAssignAdminDto;
import com.polus.fibicomp.opa.dto.OPASubmitDto;
import com.polus.fibicomp.opa.pojo.OPADisclosure;
import com.polus.fibicomp.opa.dto.OPADashboardResponseDto;
import com.polus.fibicomp.opa.dto.OPADashboardRequestDto;
import com.polus.fibicomp.opa.dto.OPADashboardDto;

@Transactional
@Service(value = "opaDaoImpl")
public class OPADaoImpl implements OPADao {

    @Autowired
    private HibernateTemplate hibernateTemplate;

    @Autowired
    private CommonDao commonDao;

  	protected static Logger logger = LogManager.getLogger(OPADaoImpl.class.getName());

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
            e.printStackTrace();
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
        hqlQuery.append("d.opaDisclosureStatusCode = :opaDisclosureStatusCode, d.updateTimestamp = :updateTimestamp,");
        hqlQuery.append("d.updateUser = :updateUser, d.dispositionStatusCode = :dispositionStatusCode ");
        hqlQuery.append("WHERE d.opaDisclosureId = :opaDisclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId", opaSubmitDto.getOpaDisclosureId());
        query.setParameter("certificationText", opaSubmitDto.getCertificationText());
        query.setParameter("certifiedBy", AuthenticatedUser.getLoginPersonId());
        query.setParameter("submissionTimestamp", timesStamp);
        query.setParameter("opaDisclosureStatusCode", Constants.OPA_DISCLOSURE_STATUS_SUBMIT);
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
        hqlQuery.append("d.opaDisclosureStatusCode = :opaDisclosureStatusCode, d.updateTimestamp = :updateTimestamp, ");
        hqlQuery.append("d.updateUser = :updateUser, d.dispositionStatusCode = :dispositionStatusCode ");
        hqlQuery.append("WHERE d.opaDisclosureId = :opaDisclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId",opaDisclosureId);
        query.setParameter("certificationText", null);
        query.setParameter("certifiedBy", null);
        query.setParameter("submissionTimestamp", null);
        query.setParameter("opaDisclosureStatusCode", opaStatusCode);
        query.setParameter("dispositionStatusCode", null);
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
        hqlQuery.append("d.updateUser = :updateUser ");
        hqlQuery.append("WHERE d.opaDisclosureId = :opaDisclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId",assignAdminDto.getOpaDisclosureId());
        query.setParameter("adminGroupId", assignAdminDto.getAdminGroupId());
        query.setParameter("adminPersonId", assignAdminDto.getAdminPersonId());
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
        hqlQuery.append("d.opaDisclosureStatusCode = :opaDisclosureStatusCode, d.updateTimestamp = :updateTimestamp, ");
        hqlQuery.append("d.updateUser = :updateUser, d.dispositionStatusCode = :dispositionStatusCode ");
        hqlQuery.append("WHERE d.opaDisclosureId = :opaDisclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId",opaDisclosureId);
        query.setParameter("opaDisclosureStatusCode", Constants.OPA_DISCLOSURE_STATUS_COMPLETED);
        query.setParameter("dispositionStatusCode", Constants.OPA_DISPOSITION_STATUS_COMPLETED);
        query.setParameter("updateTimestamp", timesStamp);
        query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
        query.executeUpdate();
        return timesStamp;
    }

    @Override
    public boolean isOPAWithStatuses(String opaDisclosureStatus, String dispositionStatus, Integer opaDisclosureId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT case when (count(d.opaDisclosureId) > 0) then true else false end FROM OPADisclosure d WHERE ");
        if (opaDisclosureStatus != null)
            hqlQuery.append(" d.opaDisclosureStatusCode = :opaDisclosureStatusCode AND  ");
        if (dispositionStatus != null)
            hqlQuery.append("d.dispositionStatusCode = :dispositionStatusCode AND ");
        hqlQuery.append("d.opaDisclosureId = :opaDisclosureId ");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId",opaDisclosureId);
        if (opaDisclosureStatus != null)
            query.setParameter("opaDisclosureStatusCode", opaDisclosureStatus);
        if (dispositionStatus != null)
            query.setParameter("dispositionStatusCode", dispositionStatus);
        return (boolean) query.getSingleResult();
    }

	@Override
	public Integer createOpaDisclosure(String personId, String homeUnit) {
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
					return resultSet.getInt("LI_OPA_DISCLOSURE_ID");
				}
			}
		} catch (Exception e) {
			logger.error("Exception in createOpaDisclosure {}", e.getMessage());
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
                        .hasPotentialConflict(rset.getBoolean("CONFLICT_DESCRIPTION"))
                        .createTimestamp(rset.getTimestamp("CREATE_TIMESTAMP"))
                        .createUser(rset.getString("CREATE_USER"))
                        .submissionTimestamp(rset.getTimestamp("SUBMISSION_TIMESTAMP"))
                        .updateTimestamp(rset.getTimestamp("UPDATE_TIMESTAMP"))
                        .updateUser(rset.getString("UPDATE_USER"))
                        .updateUserFullName(rset.getString("UPDATE_USER_FULL_NAME"))
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

    ResultSet getOPADashboardResultSet(OPADashboardRequestDto requestDto, boolean isCount) throws SQLException {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        Integer currentPage = requestDto.getCurrentPage();
        Integer pageNumber = requestDto.getPageNumber();
        CallableStatement statement = connection.prepareCall("{call GET_COI_OPA_DASHBOARD(?,?,?,?,?,?,?)}");
        statement.setString(1, AuthenticatedUser.getLoginPersonId());
        statement.setString(2, requestDto.getFilterType());
        statement.setBoolean(3, isCount);
        statement.setString(4, setOPASortOrder(requestDto.getSort()));
        statement.setInt(5, (currentPage == null ? 0 : currentPage - 1));
        statement.setInt(6, (pageNumber == null ? 0 : pageNumber));
        statement.setString(7, requestDto.getTabType());
        statement.execute();
        return  statement.getResultSet();
    }

    private String setOPASortOrder(Map<String, String> sort) {
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
}
