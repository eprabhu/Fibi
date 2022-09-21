package com.polus.fibicomp.grantcall.service;

import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.grantcall.vo.GrantCallIOIVO;

@Transactional
@Service
public interface GrantCallIOIService {

	/**
	 * This method is used to save or update grant call IOI.
	 * 
	 * @param vo - Object of GrantCallIOIVO class.
	 * @return set of values to figure out details about a grant call IOI.
	 */
	public String saveOrUpdateGrantIOIDetails(GrantCallIOIVO vo);

	/**
	 * This method is used to delete member in an IOI
	 * @param Object of GrantCallIOIVO class.
	 * @return
	 */
	public String deleteIOIMember(GrantCallIOIVO vo);

	/**
	 * This method is used to delete IOI list item 
	 * @param Object of GrantCallIOIVO class.
	 * @return
	 */
	public String deleteIOIListItem(GrantCallIOIVO vo);

	/**
	 * This method is used to delete member in an IOI 
	 * @param Object of GrantCallIOIVO class.
	 * @return
	 */
	public String loadGrantCallIOIByGrantId(GrantCallIOIVO vo);

	/**
	 * This method is used to add member in an IOI 
	 * @param Object of GrantCallIOIVO class.
	 * @return
	 */
	public String saveOrUpdateIOIMembers(GrantCallIOIVO vo);

	/**
	 * This method is used while creating or editing a GrantCallIOI
	 * @param Object of GrantCallIOIVO class.
	 * @return
	 */
	public String createOrEditGrantCallIOI(GrantCallIOIVO vo);
	
	/**
	 * This method is used for exporting the IOI details
	 * @param Object of GrantCallIOIVO class.
	 * @return
	 */
	public XSSFWorkbook getXSSFWorkbookForIOI(GrantCallIOIVO vo) throws Exception;

	/**
	 * This method is used to get excel sheet in byte array format.
	 * 
	 * @param XSSFWorkbook for excel sheet.
	 * @return ResponseEntity<byte[]> that contains data in byte array.
	 * @throws Exception
	 */
	public ResponseEntity <byte[]> getResponseEntityForIOIDownload(GrantCallIOIVO vo, XSSFWorkbook workbook) throws Exception;

}
