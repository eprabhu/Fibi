package com.polus.fibicomp.currentandpending.dao;

import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.currentandpending.pojo.CPReportHeader;
import com.polus.fibicomp.currentandpending.pojo.CPReportProjectDetail;
import com.polus.fibicomp.currentandpending.pojo.CPReportProjectDetailExt;

@Service
public interface CurrentAndPendingDao {

	/**
	 * This method is used to save CPReportHeader details.
	 * @param cpReportHeader
	 * @return it returns the CPReportHeader details.
	 */
	public CPReportHeader saveOrUpdateCPReportHeader(CPReportHeader cpReportHeader);

	/**
	 * This method is used to check if the report is generated for this person.
	 * @param personId
	 * @param moduleItemId
	 * @return it returns boolean value.
	 */
	public Boolean checkIfReportGenerated(String personId, String moduleItemId);

	/**
	 * This method is used to save CPReportProjectDetail details.
	 * @param cpReportProjectDetail
	 * @return it returns the CPReportProjectDetail details.
	 */
	public CPReportProjectDetail saveOrUpdateCPReportProjectDetail(CPReportProjectDetail cpReportProjectDetail);

	/**
	 * This method is used to fetch Maximum of VersionNumber Based On ModuleItemId.
	 * @param moduleItemId
	 * @param personId
	 * @return it returns version number.
	 */
	public Integer getMaxVersionNumberByParams(String moduleItemId, String personId);

	/**
	 * This method is used to save CPReportProjectDetailExt details.
	 * @param cpReportProjectDetailExt
	 * @return it returns the CPReportProjectDetailExt details.
	 */
	public CPReportProjectDetailExt saveOrUpdateCPReportProjectDetailExt(CPReportProjectDetailExt cpReportProjectDetailExt);

	/**
	 * This method is used to save fetch CPReportHeader Details By Id.
	 * @param cpReportHeaderId
	 * @return it returns the CPReportHeader details.
	 */
	public CPReportHeader fetchCPReportHeaderDetailsById(Integer cpReportHeaderId);

	/**
	 * This method is used to save fetch CPReportHeader Details By Id.
	 * @param cpReportHeaderId
	 * @return it returns the CPReportHeader details.
	 */
	public List<String> fetchPersonIdsByParams(Integer moduleCode, String moduleItemId);

	/**
	 * This method is used to  fetch the max version of CPReportHeader Detail of a person By Id.
	 * @param moduleCode
	 * @param moduleItemId
	 * @param personId
	 * @return it returns the CPReportHeader details.
	 */
	public CPReportHeader getCPReportHeaderOfMaxVersionOfPerson(Integer moduleCode, String moduleItemId, String personId);

	/**
	 * This method is used to save fetch CPReportProjectDetail By Id.
	 * @param cpReportHeaderId
	 * @return it returns the list of CPReportProjectDetail details.
	 */
	public List<CPReportProjectDetail> getCPReportProjectDetailsByCPReportHeaderId(Integer cpReportHeaderId);

	/**
	 * This method is used to save fetch CPReportProjectDetailExt By Id.
	 * @param cpReportProjectDetailId
	 * @return it returns the CPReportProjectDetailExt details.
	 */
	public CPReportProjectDetailExt getCPReportProjectDetailExtById(Integer cpReportProjectDetailId);

	/**
	 * This method is used to fetch CPReportProject Details By Id.
	 * @param cpReportProjectDetailId
	 * @return it returns the CPReportProjectDetail details.
	 */
	public CPReportProjectDetail getCPReportProjectDetailsById(Integer cpReportProjectDetailId);

	/**
	 * This method is used to save fetch CPReportHeader Details By personId, nonEmployeeFlag, moduleCode, moduleItemId.
	 * @param personId
	 * @param nonEmployeeFlag
	 * @param moduleCode
	 * @param moduleItemId
	 * @return it returns the list of CPReportHeader details.
	 */
	public List<CPReportHeader> fetchCPReportHeadersByParams(String personId, Boolean nonEmployeeFlag, Integer moduleCode, String moduleItemId);

	/**
	 * This method is used to fetch CPReportProject Details By Id.
	 * @param cpReportHeaderId
	 * @return it returns list of CPProjectDetailIds.
	 */
	public List<Integer> getCPProjectDetailIdsByCPReportHeaderId(Integer cpReportHeaderId);

	/**
	 * This method is used to delete list of CPReportHeaders.
	 * @param cpReportHeaders
	 */
	public void deleteCPReportHeaders(List<CPReportHeader> cpReportHeaders);

	/**
	 * This method is used to delete CPProjectDetailExt detail.
	 * @param CPProjectDetailExt
	 */
	public void deleteCPProjectDetailExt(CPReportProjectDetailExt cpReportProjectDetailExt);

	/**
	 * This method is used to delete external funding support detail.
	 * @param cpReportProjectDetailId
	 * @return success message
	 */
	public String deleteCPExternalProjectDetail(Integer cpReportProjectDetailId);

}
