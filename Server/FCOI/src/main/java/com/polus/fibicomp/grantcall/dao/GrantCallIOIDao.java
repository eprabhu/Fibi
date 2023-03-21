package com.polus.fibicomp.grantcall.dao;

import java.util.List;
import java.util.Set;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.grantcall.pojo.GrantCallIOIHeader;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIMembers;

@Transactional
@Service
public interface GrantCallIOIDao {

	/**
	 * This method is used to save or update grant call IOI.
	 * 
	 * @param grantCall - Object of GrantCallIOIHeader.
	 * @return set of values to figure out details about a grant call IOI.
	 */
	public GrantCallIOIHeader saveOrUpdateGrantCallIOI(GrantCallIOIHeader grantCallIOI);

	/**
	 * This method is used to delete an IOIMember.
	 * @param An object of grantCallIOIMember.
	 * @return
	 */
	public void deleteIOIMember(GrantCallIOIMembers grantCallIOIMember);

	/**
	 * This method is used to fetch IOIMember Based On grantIOIMemberId.
	 * @param grantIOIMemberId - grantIOIMemberId of the IOIMembers.
	 * @return An object of IOIMembers.
	 */
	public GrantCallIOIMembers fetchIOIMember(Integer grantIOIMemberId);

	/**
	 * This method is used to fetch IOIMember Based On IOIId.
	 * @param grantCallIOIId - grantCallIOIId of the proposal.
	 * @return An object of IOIMembers.
	 */
	public List<GrantCallIOIMembers> fetchIOIMembersBasedOnIOIId(Integer grantCallIOIId);

	/**
	 * This method is used to fetch IOIdetails Based On IOIId.
	 * @param grantCallIOIId - grantCallIOIId of the IOI.
	 * @return An object of grantCallIOIHeader.
	 */
	public GrantCallIOIHeader fetchGrantIOIByIOIId(Integer grantIOIId);

	/**
	 * This method is used to delete a GrantCallIOI.
	 * @param An object of grantCallIOIHeader.
	 * @return
	 */
	public void deleteGrantCallIOI(GrantCallIOIHeader grantCallIOIHeader);

	/**
	 * This method is used to save or update IOIMember.
	 * @param grantCallIOIMembers - GrantCallIOIMembers of the IOI.
	 * @return An object of IOIMembers.
	 */
	public GrantCallIOIMembers saveOrUpdateIOIMembers(GrantCallIOIMembers grantCallIOIMember);

//	/**
//	 * This method is used to fetch PersonId based on UnitId & RoleId.
//	 * @param An object of unitNumber,roleId.
//	 * @return An string of personId.
//	 */
//	public Set<String> fetchPersonIdByUnitIdAndRoleId(String unitNumber,Integer roleId);

	/**
	 * This method is used to fetch dashboard data for GrantCallIOIHeader
	 * @param grantCallId
	 * @param personId
	 * @param tabName
	 * @return List of GrantCallIOIHeader
	 */
	public List<GrantCallIOIHeader> getDashboardDataForIOI(Integer grantCallId, String personId, String tabName);

	/**
	 * This method is used to fetch submitted IOI list based on grantCallId and status code.
	 * @param grantCallId - grantCallId of the GrantCall.
	 * @param statusCode - statusCode
	 * @return Grant call IOI List.
	 */
	public List<GrantCallIOIHeader> fetchSubmittedGrantCallIOIByGrantCallId(Integer grantCallId, Integer grantIOIStatusCode);

}
