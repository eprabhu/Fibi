package com.polus.fibicomp.roles.dao;

import java.io.Serializable;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.proposal.pojo.ProposalPersonRoles;
import com.polus.fibicomp.roles.pojo.ModuleDerivedRoles;
import com.polus.fibicomp.roles.pojo.PersonRoles;
import com.polus.fibicomp.roles.pojo.Rights;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.roles.pojo.RoleRights;
import com.polus.fibicomp.roles.pojo.RoleType;
import com.polus.fibicomp.roles.vo.RoleManagementVO;
import com.polus.fibicomp.utils.DashBoardQueries;
import com.polus.fibicomp.view.RolesView;
import com.polus.fibicomp.common.dao.CommonDao;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "rolesManagement")
public class RolesManagementDaoImpl implements RolesManagementDao {

	protected static Logger logger = LogManager.getLogger(RolesManagementDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	@Value("${oracledb}")
	private String oracledb;

	private static final String PERSON_ID = "personId";
	private static final String ROLE_ID = "roleId";
	private static final String RIGHT_ID = "rightId";
	private static final String UNIT_NUMBER = "unitNumber";


	@Override
	public List<PersonRoles> getAssignedRoleOfPerson(String personId, String unitNumber, Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonRoles> query = builder.createQuery(PersonRoles.class);
		Root<PersonRoles> personRole = query.from(PersonRoles.class);
		Predicate predicate1 = builder.equal(personRole.get(PERSON_ID), personId);
		Predicate predicate2 = builder.equal(personRole.get(UNIT_NUMBER), unitNumber);
		Predicate predicate3 = builder.equal(personRole.get(ROLE_ID), roleId);
		if (personId != null && unitNumber != null && roleId != null) {
			query.where(builder.and(predicate1, predicate2, predicate3));
		} else if (personId != null && unitNumber != null) {
			query.where(builder.and(predicate1, predicate2));
		} else if (unitNumber != null && roleId != null) {
			query.where(builder.and(predicate2, predicate3));
		} else if (personId != null && roleId != null) {
			query.where(builder.and(predicate1, predicate3));
		} else if (personId != null) {
			query.where(builder.and(predicate1));
		} else if (unitNumber != null) {
			query.where(builder.and(predicate2));
		} else if (roleId != null) {
			query.where(builder.and(predicate3));
		}
		return session.createQuery(query).list();
	}

	@Override
	public List<Role> getUnAssignedRoleOfPerson(String personId, String unitNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Role> outerQuery = builder.createQuery(Role.class);
		Root<Role> role = outerQuery.from(Role.class);
		Subquery<PersonRoles> subQuery = outerQuery.subquery(PersonRoles.class);
		Root<PersonRoles> personRole = subQuery.from(PersonRoles.class);
		Predicate predicate1 = builder.equal(personRole.get(PERSON_ID), personId);
		Predicate predicate2 = builder.equal(personRole.get(UNIT_NUMBER), unitNumber);
		subQuery.select(personRole.get(ROLE_ID)).where(builder.and(predicate1, predicate2));
		outerQuery.select(role).where(builder.in(role.get(ROLE_ID)).value(subQuery).not());
		return session.createQuery(outerQuery).list();
	}

	@Override
	public Role getRoleInformation(Integer roleId) {
		return hibernateTemplate.get(Role.class, roleId);
	}

	@Override
	public List<Rights> getAllRightsForRole(Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Rights> outerQuery = builder.createQuery(Rights.class);
		Root<Rights> right = outerQuery.from(Rights.class);
		Subquery<RoleRights> subQuery = outerQuery.subquery(RoleRights.class);
		Root<RoleRights> roleRight = subQuery.from(RoleRights.class);
		Predicate predicate1 = builder.equal(roleRight.get(ROLE_ID), roleId);
		subQuery.select(roleRight.get(RIGHT_ID)).where(builder.and(predicate1));
		outerQuery.select(right).where(builder.in(right.get(RIGHT_ID)).value(subQuery));
		return session.createQuery(outerQuery).list();
	}

	@Override
	public Integer insertPersonRole(PersonRoles personRoles) {
		Serializable id = hibernateTemplate.save(personRoles);
		return Integer.parseInt(id.toString());
	}

	@Override
	public void updatePersonRole(PersonRoles personRoles) {
		hibernateTemplate.update(personRoles);
	}

	@Override
	public void deletePersonRole(PersonRoles personRoles) {
		hibernateTemplate.delete(personRoles);
	}

	@Override
	public List<Role> fetchAllRoles() {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Role> criteria = builder.createQuery(Role.class);
			Root<Role> roleRoot = criteria.from(Role.class);
			criteria.orderBy(builder.asc(roleRoot.get("roleName")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public String getAssignedRole(RoleManagementVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query roleList = null;
		if (vo.getUnitNumber() != null && vo.getRoleId() != null && vo.getPersonId() == null) {
			roleList = session.createSQLQuery(DashBoardQueries.ROLE_WITH_UNIT_AND_ROLE);
			roleList.setParameter(UNIT_NUMBER, vo.getUnitNumber());
			roleList.setParameter(ROLE_ID, vo.getRoleId());
		} else if (vo.getUnitNumber() != null && vo.getRoleId() == null && vo.getPersonId() == null) {
			roleList = session.createSQLQuery(DashBoardQueries.ROLE_WITH_UNIT);
			roleList.setParameter(UNIT_NUMBER, vo.getUnitNumber());
		} else if (vo.getRoleId() != null && vo.getUnitNumber() == null && vo.getPersonId() == null) {
			roleList = session.createSQLQuery(DashBoardQueries.ROLE_WITH_ROLE);
			roleList.setParameter(ROLE_ID, vo.getRoleId());
		} else if (vo.getPersonId() != null && vo.getUnitNumber() == null && vo.getRoleId() == null) {
			roleList = session.createSQLQuery(DashBoardQueries.ROLES_OF_PERSON);
			roleList.setParameter(PERSON_ID, vo.getPersonId());
		} else if (vo.getPersonId() != null && vo.getUnitNumber() != null && vo.getRoleId() == null) {
			roleList = session.createSQLQuery(DashBoardQueries.ROLE_WITH_UNIT_AND_PERSON);
			roleList.setParameter(PERSON_ID, vo.getPersonId());
			roleList.setParameter(UNIT_NUMBER, vo.getUnitNumber());
		} else if (vo.getPersonId() != null && vo.getUnitNumber() != null && vo.getRoleId() != null) {
			roleList = session.createSQLQuery(DashBoardQueries.ROLE_WITH_UNIT_AND_PERSON_AND_ROLE);
			roleList.setParameter(PERSON_ID, vo.getPersonId());
			roleList.setParameter(UNIT_NUMBER, vo.getUnitNumber());
			roleList.setParameter(ROLE_ID, vo.getRoleId());
		} else if (vo.getPersonId() != null && vo.getUnitNumber() == null && vo.getRoleId() != null) {
			roleList = session.createSQLQuery(DashBoardQueries.ROLE_WITH_ROLE_AND_PERSON);
			roleList.setParameter(PERSON_ID, vo.getPersonId());
			roleList.setParameter(ROLE_ID, vo.getRoleId());
		} else {
			roleList = session.createSQLQuery(DashBoardQueries.FETCH_ALL_ROLES);
		}
		@SuppressWarnings("unchecked")
		List<Object[]> roles = roleList.getResultList();
		List<RolesView> rolesViews = new ArrayList<>();
		for (Object[] role : roles) {
			RolesView rolesView = new RolesView();
			rolesView.setPersonId(role[0].toString());
			rolesView.setFullName(role[1].toString());
			if (role[2] != null) {
				rolesView.setEmail(role[2].toString());
			}
			rolesView.setUserName(role[3] == null ? null :role[3].toString());
			rolesView.setUnitName(role[4].toString());
			rolesView.setUnitNumber(role[5].toString());
			rolesView.setPrimaryTitle(role[6] == null ? null : role[6].toString());
			rolesView.setDirectoryTitle(role[7] == null ? null : role[7].toString());
			rolesViews.add(rolesView);
		}
		return commonDao.convertObjectToJSON(rolesViews);
	}

	@Override
	public List<Rights> fetchAllRights() {
		return hibernateTemplate.loadAll(Rights.class);
	}

	@Override
	public List<RoleType> fetchAllRoleTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<RoleType> query = builder.createQuery(RoleType.class);
		Root<RoleType> rootUnit = query.from(RoleType.class);
		query.orderBy(builder.asc(rootUnit.get("roleType")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Integer saveRole(Role role) {
		Serializable id = hibernateTemplate.save(role);
		return Integer.parseInt(id.toString());
	}

	@Override
	public void saveRoleRight(RoleRights roleRight) {
		hibernateTemplate.saveOrUpdate(roleRight);
	}

	@Override
	public List<RoleRights> fetchRoleRightsByRoleId(Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<RoleRights> query = builder.createQuery(RoleRights.class);
		Root<RoleRights> roleRight = query.from(RoleRights.class);
		Predicate predicate1 = builder.equal(roleRight.get(ROLE_ID), roleId);
		query.where(builder.and(predicate1));
		return session.createQuery(query).list();
	}

	@Override
	public void deleteRole(Integer roleId) {
		try {
			hibernateTemplate.delete(hibernateTemplate.get(Role.class, roleId));
		} catch (Exception e) {
			logger.error("Error occured in deleteRole : {}", e.getMessage());
			e.printStackTrace();
		}
	}

	@Override
	public void deleteRoleRights(List<RoleRights> roleRights) {
		try {
			hibernateTemplate.deleteAll(roleRights);
		} catch (Exception e) {
			logger.error("Error occured in deleteRoleRights : {}", e.getMessage());
			e.printStackTrace();
		}
	}

	@Override
	public void deleteRoleRight(Integer roleRightId) {
		try {
			hibernateTemplate.delete(hibernateTemplate.get(RoleRights.class, roleRightId));
		} catch (Exception e) {
			logger.error("Error occured in deleteRoleRight : {}", e.getMessage());
			e.printStackTrace();
		}
	}

	@Override
	public void updateRole(Role role) {
		hibernateTemplate.update(role);
	}

	@Override
	public List<Role> getRoleBasedOnRoleId(List<Integer> roleIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Role> query = builder.createQuery(Role.class);
		Root<Role> role = query.from(Role.class);
		Predicate predicate1 = builder.in(role.get(ROLE_ID)).value(roleIds);
		query.where(builder.and(predicate1));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Rights> fetchUnAssignedRights(Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Rights> outerQuery = builder.createQuery(Rights.class);
		Root<Rights> right = outerQuery.from(Rights.class);	
		Subquery<RoleRights> subQuery = outerQuery.subquery(RoleRights.class);
		Root<RoleRights> roleRight = subQuery.from(RoleRights.class);			
		Predicate predicate1 = builder.equal(roleRight.get(ROLE_ID), roleId);
		subQuery.select(roleRight.get(RIGHT_ID)).where(builder.and(predicate1));
		outerQuery.select(right).where(builder.in(right.get(RIGHT_ID)).value(subQuery).not());
		return session.createQuery(outerQuery).list();
	}

	@Override
	public Integer getRoleRightIdByRoleAndRightId(Integer roleId, Integer rightId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<RoleRights> query = builder.createQuery(RoleRights.class);
		Root<RoleRights> roleRight = query.from(RoleRights.class);			
		Predicate predicate1 = builder.equal(roleRight.get(ROLE_ID), roleId);
		Predicate predicate2 = builder.equal(roleRight.get(RIGHT_ID), rightId);
		query.where(builder.and(predicate1,predicate2));
		RoleRights roleRights = session.createQuery(query).uniqueResult();
		return roleRights.getRoleRightId();
	}

	@Override
	public void deletePersonRoles(List<PersonRoles> personRoles) {
		try {
			hibernateTemplate.deleteAll(personRoles);
		} catch (Exception e) {
			logger.error("Error occured in deletePersonRoles : {}", e.getMessage());
			e.printStackTrace();
		}
	}

	@Override
	public List<PersonRoles> getPersonRolesByRoleId(Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonRoles> query = builder.createQuery(PersonRoles.class);
		Root<PersonRoles> personRole = query.from(PersonRoles.class);
		Predicate predicate1 = builder.equal(personRole.get(ROLE_ID), roleId);
		query.where(builder.and(predicate1));
		return session.createQuery(query).list();
	}

	@Override
	public Boolean isPersonHasRole(String personId, Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT COUNT(*) FROM PersonRoles WHERE personId=:personId AND roleId=:roleId";
		Query query = session.createQuery(hqlQuery);
		query.setParameter(PERSON_ID, personId);
		query.setParameter(ROLE_ID, roleId);
		return Integer.parseInt(query.getSingleResult().toString()) > 0;
	}

	@Override
	public Boolean isPersonHasRightInAnyDepartment(String personId, String rightName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String query = "SELECT COUNT(*) FROM PERSON_ROLES T1 INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID WHERE T1.PERSON_ID = :personId AND T3.RIGHT_NAME = :rightName";
		Query value = session.createSQLQuery(query);
		value.setParameter(PERSON_ID, personId);
		value.setParameter("rightName", rightName);
		return Integer.parseInt(value.getSingleResult().toString()) > 0;
	}

	@Override
	public String deleteProposalPersons(List<ProposalPersonRoles> personRole) {
		hibernateTemplate.deleteAll(personRole);
		return "Proposal Person Role deleted successfully";
	}

	@Override
	public void syncPersonRole(String personId, String unitNumber, Integer roleId, Integer rightId, String descentFlag, String updateUser, String avType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call PERSON_ROLE_SYNCH (?,?,?,?,?,?,?)}");
				statement.setString(1, personId);
				statement.setString(2, unitNumber);
				statement.setInt(3, roleId);
				if (rightId == null) {
					statement.setNull(4, java.sql.Types.INTEGER);
				} else {
					statement.setInt(4, rightId);
				}
				statement.setString(5, descentFlag);
				statement.setString(6, updateUser);
				statement.setString(7, avType);
				statement.execute();
				statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "PERSON_ROLE_SYNCH ";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, personId);
				statement.setString(3, unitNumber);
				statement.setInt(4, roleId);
				if (rightId == null) {
					statement.setNull(5, java.sql.Types.INTEGER);
				} else {
					statement.setInt(5, rightId);
				}
				statement.setString(6, descentFlag);
				statement.setString(7, updateUser);
				statement.setString(8, avType);
				statement.execute();
				statement.getObject(1);
			}
		} catch (Exception e) {
			logger.error("Error occured in syncPersonRole : {}", e.getMessage());
			e.printStackTrace();
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
	}

	@Override
	public List<ModuleDerivedRoles> getModuleDerivedRoles(Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ModuleDerivedRoles> query = builder.createQuery(ModuleDerivedRoles.class);
		Root<ModuleDerivedRoles> personRole = query.from(ModuleDerivedRoles.class);
		Predicate predicateModule = builder.equal(personRole.get("moduleCode"), moduleCode);
		query.where(builder.and(predicateModule));
		query.orderBy(builder.asc(personRole.get("roleName")));
		return session.createQuery(query).list();
	}

	@Override
	public List<ModuleDerivedRoles> grantModuleDerivedRolesForPI(Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ModuleDerivedRoles> query = builder.createQuery(ModuleDerivedRoles.class);
		Root<ModuleDerivedRoles> personRole = query.from(ModuleDerivedRoles.class);
		Predicate predicateModule = builder.equal(personRole.get("moduleCode"), moduleCode);
		Predicate predicatePI = builder.equal(personRole.get("autoGrantPI"), "Y");
		query.where(builder.and(predicateModule,predicatePI));
		return session.createQuery(query).list();
	}

	@Override
	public List<ModuleDerivedRoles> grantModuleDerivedRolesForCOI(Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ModuleDerivedRoles> query = builder.createQuery(ModuleDerivedRoles.class);
		Root<ModuleDerivedRoles> personRole = query.from(ModuleDerivedRoles.class);
		Predicate predicateModule = builder.equal(personRole.get("moduleCode"), moduleCode);
		Predicate predicateCOI = builder.equal(personRole.get("autoGrantCOI"), "Y");
		query.where(builder.and(predicateModule,predicateCOI));
		return session.createQuery(query).list();
	}

	@Override
	public List<ModuleDerivedRoles> grantModuleDerivedRolesForCreator(Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ModuleDerivedRoles> query = builder.createQuery(ModuleDerivedRoles.class);
		Root<ModuleDerivedRoles> personRole = query.from(ModuleDerivedRoles.class);
		Predicate predicateModule = builder.equal(personRole.get("moduleCode"), moduleCode);
		Predicate predicateCreator = builder.equal(personRole.get("autoGrantModuleCreator"), "Y");
		query.where(builder.and(predicateModule,predicateCreator));
		return session.createQuery(query).list();
	}

	@Override
	public void roleRIghtAuditTab(String source, Integer oldRoleId, Integer oldRightId, 
			String personId, String oldUnitNumber, String descentFlag, String updateUser) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call ROLE_RIGHTS_AUDIT_PROC (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, source);
				statement.setString(2, "DELETE");
				statement.setNull(3, java.sql.Types.INTEGER);
				statement.setNull(4, java.sql.Types.INTEGER);
				statement.setNull(5, java.sql.Types.INTEGER);
				statement.setNull(6, java.sql.Types.INTEGER);
				statement.setString(7, personId);
				statement.setString(8, null);
				statement.setInt(9, oldRoleId);
				statement.setNull(10, java.sql.Types.INTEGER);
				if(oldRightId != null) {
					statement.setInt(11, oldRightId);
				} else {
					statement.setNull(11, java.sql.Types.INTEGER);
				}
				statement.setNull(12, java.sql.Types.INTEGER);
				statement.setString(13, oldUnitNumber);
				statement.setString(14, null);
				statement.setString(15, descentFlag);
				statement.setString(16, null);
				statement.setString(17, updateUser);
				statement.execute();
				statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "ROLE_RIGHTS_AUDIT_PROC ";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, source);
				statement.setString(3, "DELETE");
				statement.setNull(4, java.sql.Types.INTEGER);
				statement.setNull(5, java.sql.Types.INTEGER);
				statement.setNull(6, java.sql.Types.INTEGER);
				statement.setNull(7, java.sql.Types.INTEGER);
				statement.setString(8, personId);
				statement.setString(9, null);
				statement.setInt(10, oldRoleId);
				statement.setNull(11, java.sql.Types.INTEGER);
				statement.setInt(12, oldRightId);
				statement.setNull(13, java.sql.Types.INTEGER);
				statement.setString(14, oldUnitNumber);
				statement.setString(15, null);
				statement.setString(16, descentFlag);
				statement.setString(17, null);
				statement.setString(18, updateUser);
				statement.execute();
				statement.getObject(1);
			}
		} catch (Exception e) {
			logger.error("Error occured in roleRIghtAuditTab : {}", e.getMessage());
			e.printStackTrace();
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
	}
}
