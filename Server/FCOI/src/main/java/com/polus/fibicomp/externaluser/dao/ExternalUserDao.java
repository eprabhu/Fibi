package com.polus.fibicomp.externaluser.dao;

import java.util.List;

import javax.transaction.Transactional;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.externaluser.pojo.ExternalUser;
import com.polus.fibicomp.externaluser.pojo.ExternalUserFeed;
import com.polus.fibicomp.person.pojo.Person;

@Transactional
@Service
public interface ExternalUserDao {

	public List<ExternalUser> fetchAllExternalUserDetails(List<String> unit);

	public void updateApproveReject(ExternalUser externalUser);

	public ExternalUser getExternalUserByUserName(String userName);

	public ExternalUser getExternalUserByPersonId(Integer personId);

	public ExternalUserFeed saveExternalUserFeed(ExternalUserFeed userFeed);

	public List<ExternalUserFeed> getExternalUserFeedDetailsForAdd();

	public void saveOrUpdate(ExternalUserFeed userDetails);

	public List<ExternalUserFeed> getExternalUserFeedDetailsForDelete();

	public List<ExternalUserFeed> getExternalUserFeeds();

	public void assignPersonRole(Person person, Integer roleId);

	public void assignPersonRoleRT(Person person, Integer roleId);

	public String getHomeUnitFromOrganizationId(String organizationId);

	public List<ExternalUser> getPendingExternalUserList();

	public List<String> getUnitsListByPersonIdAndRights(String personId);
}
