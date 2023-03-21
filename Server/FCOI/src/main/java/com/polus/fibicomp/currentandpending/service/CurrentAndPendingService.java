package com.polus.fibicomp.currentandpending.service;

import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.currentandpending.dto.CurrentAndPendingPersonDTO;
import com.polus.fibicomp.currentandpending.vo.CurrentAndPendingVO;

@Service
public interface CurrentAndPendingService {

	/**
	 * This method is used to fetch key persons.
	 * @param currentAndPendingVO
	 * @return it returns the key person list.
	 */
	public String getPersonList(CurrentAndPendingVO currentAndPendingVO);

	/**
	 * This method is used to fetch CurrentAndPending List.
	 * @param currentAndPendingVO
	 * @return it returns the CurrentAndPendingList.
	 */
	public String getCPDetailsForSelectedPersons(CurrentAndPendingVO currentAndPendingVO);

	/**
	 * This method is used to save CPReportProjectDetailExt Detail.
	 * @param currentAndPendingVO
	 * @return currentAndPendingVO.
	 */
	public String saveOrUpdateCPReportExtDetail(CurrentAndPendingVO currentAndPendingVO);

	/**
	 * This method is used to exclude Current and pending detail.
	 * @param currentAndPendingVO
	 * @return currentAndPendingVO .
	 */
	public String excludeCurrentAndPendingDetails(CurrentAndPendingVO currentAndPendingVO);

	/**
	 * This method is used to exclude Current and pending detail.
	 * @param currentAndPendingVO
	 * @return currentAndPendingVO .
	 */
	public String getCurrentAndPendingDetails(CurrentAndPendingVO currentAndPendingVO);

	/**
	 * This method is used to exclude Current and pending detail.
	 * @param currentAndPendingVO
	 * @return currentAndPendingVO .
	 */
	public List<CurrentAndPendingPersonDTO> preparePersonCurrentAndPendingDetails(CurrentAndPendingVO currentAndPendingVO);

	/**
	 * This method is used to save external Project Details which is manulaly added.
	 * @param currentAndPendingVO
	 * @return currentAndPendingVO .
	 */
	public String saveOrUpdateCPExternalProjectDetail(CurrentAndPendingVO currentAndPendingVO);

	/**
	 * This method is used to delete CPExternalProjectDetail based on CPExternalProjectDetail id.
	 * @param currentAndPendingVO - CurrentAndPendingVO.
	 * @return success message.
	 */
	public String deleteCPExternalProjectDetail(CurrentAndPendingVO currentAndPendingVO);

}