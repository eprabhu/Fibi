package com.polus.fibicomp.coi.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Types;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Query;

import com.polus.core.applicationexception.dto.ApplicationException;
import com.polus.core.common.dao.CommonDao;
import com.polus.core.security.AuthenticatedUser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.constants.Constants;

import oracle.jdbc.OracleTypes;

@Repository
@Transactional
public class GeneralDaoImpl implements GeneralDao {

    protected static Logger logger = LogManager.getLogger(GeneralDaoImpl.class.getName());

    @Autowired
    private HibernateTemplate hibernateTemplate;

    @Autowired
    private CommonDao commonDao;

    @Value("${oracledb}")
    private String oracledb;

    @Override
    public boolean isPersonInReviewer(String personId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("select (CASE WHEN count(cr.coiReviewId) > 0 THEN true ELSE false END) from CoiReview cr where cr.assigneePersonId = :personId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("personId", personId);
        return (Boolean) query.getSingleResult();
    }

	@Override
	public List<String> fetchAllCoiOpaRights(String loginPersonId) {
		List<String> rights = new ArrayList<>();
		rights.addAll(fetchRightsByModule(loginPersonId, Constants.COI_MODULE_CODE));
		rights.addAll(fetchRightsByModule(loginPersonId, Constants.OPA_MODULE_CODE));
		rights.addAll(fetchRightsByModule(loginPersonId, Constants.TRAVEL_MODULE_CODE));
		rights.addAll(fetchRightsByModule(loginPersonId, Constants.CONSULT_DISCL_MODULE_CODE));
		return rights;
	}

	private List<String> fetchRightsByModule(String loginPersonId, Integer moduleCode) {
		List<String> rightList = new ArrayList<>();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement = null;
        ResultSet rset = null;
        try {
            if (oracledb.equalsIgnoreCase("N")) {
                statement = connection.prepareCall("{call GET_ALL_RIGHTS_FOR_A_MODULE(?,?,?,?)}");
                statement.setInt(1, moduleCode);
                statement.setString(2, loginPersonId);
                statement.setString(3, AuthenticatedUser.getLoginPersonUnit());
                statement.setNull(4, Types.DECIMAL);
                statement.execute();
                rset = statement.getResultSet();
            } else if (oracledb.equalsIgnoreCase("Y")) {
                String functionCall = "{call GET_ALL_RIGHTS_FOR_A_MODULE(?,?,?,?,?)}";
                statement = connection.prepareCall(functionCall);
                statement.registerOutParameter(1, OracleTypes.CURSOR);
                statement.setInt(2, moduleCode);
                statement.setString(3, loginPersonId);
                statement.setString(4, AuthenticatedUser.getLoginPersonUnit());
                statement.setNull(5, Types.DECIMAL);
                statement.execute();
                rset = (ResultSet) statement.getObject(1);
            }
            while (rset != null && rset.next()) {
                rightList.add(rset.getString("RIGHT_NAME"));
            }
        } catch (Exception e) {
           logger.error("Exception on fetchRightsByModule {}", e.getMessage());
           throw new ApplicationException("Unable to fetch rights", e, Constants.JAVA_ERROR);
        }
        return rightList;
	}

    @Override
    public boolean isPersonInOPAReviewer(String personId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("select (CASE WHEN count(cr.opaReviewId) > 0 THEN true ELSE false END) ");
        hqlQuery.append("FROM OPAReview cr where cr.assigneePersonId = :personId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("personId", personId);
        return (Boolean) query.getSingleResult();
    }
}
