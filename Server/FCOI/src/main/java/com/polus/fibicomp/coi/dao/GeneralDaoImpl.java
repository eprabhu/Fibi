package com.polus.fibicomp.coi.dao;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.security.AuthenticatedUser;
import oracle.jdbc.OracleTypes;
import oracle.jdbc.driver.Const;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.Query;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

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
    public List<String> fetchAllCoiRights(String personId) {
        List<String> rightList = new ArrayList<>();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement = null;
        ResultSet rset = null;
        try {
            if (oracledb.equalsIgnoreCase("N")) {
                statement = connection.prepareCall("{call GET_ALL_RIGHTS_FOR_A_MODULE(?,?,?,?)}");
                statement.setInt(1, Constants.COI_MODULE_CODE);
                statement.setString(2, personId);
                statement.setString(3, AuthenticatedUser.getLoginPersonUnit());
                statement.setNull(4, Types.DECIMAL);
                statement.execute();
                rset = statement.getResultSet();
            } else if (oracledb.equalsIgnoreCase("Y")) {
                String functionCall = "{call GET_ALL_RIGHTS_FOR_A_MODULE(?,?,?,?,?)}";
                statement = connection.prepareCall(functionCall);
                statement.registerOutParameter(1, OracleTypes.CURSOR);
                statement.setInt(2, Constants.COI_MODULE_CODE);
                statement.setString(3, personId);
                statement.setString(4, AuthenticatedUser.getLoginPersonUnit());
                statement.setNull(5, Types.DECIMAL);
                statement.execute();
                rset = (ResultSet) statement.getObject(1);
            }
            while (rset != null && rset.next()) {
                rightList.add(rset.getString("RIGHT_NAME"));
            }
        } catch (Exception e) {
           logger.error("Exception on fetchAllCoiRights {}", e.getMessage());
           throw new ApplicationException("Unable to fetch rights", e, Constants.JAVA_ERROR);
        }
        return rightList;
    }

    @Override
    public boolean isPersonInReviewer(String personId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("select (CASE WHEN count(cr.coiReviewId) > 0 THEN true ELSE false END) from CoiReview cr where cr.assigneePersonId = :personId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("personId", personId);
        return (Boolean) query.getSingleResult();
    }
}
