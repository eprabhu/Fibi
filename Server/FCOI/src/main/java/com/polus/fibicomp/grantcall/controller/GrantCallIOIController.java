package com.polus.fibicomp.grantcall.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.grantcall.service.GrantCallIOIService;
import com.polus.fibicomp.grantcall.service.GrantCallService;
import com.polus.fibicomp.grantcall.vo.GrantCallIOIVO;

@RestController
public class GrantCallIOIController {

	protected static Logger logger =  LogManager.getLogger(GrantCallIOIController.class.getName());

	@Autowired
	@Qualifier(value = "grantCallService")
	private GrantCallService grantCallService;

	@Autowired
	@Qualifier(value = "grantCallIOIService")
	private GrantCallIOIService grantCallIOIService;

	@RequestMapping(value = "/createOrEditGrantCallIOI", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createGrantCallIOI(@RequestBody GrantCallIOIVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createOrEditGrantCallIOI");
		return grantCallIOIService.createOrEditGrantCallIOI(vo);
	}

	@RequestMapping(value = "/saveOrUpdateGrantIOIDetails", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveUpdateGrantCall(@RequestBody GrantCallIOIVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateGrantIOIDetails");
		return grantCallIOIService.saveOrUpdateGrantIOIDetails(vo);
	}

	@RequestMapping(value = "/loadGrantCallIOIByGrantId", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadGrantCallIOIByGrantId(@RequestBody GrantCallIOIVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadGrantCallIOIByGrantId");
		logger.info("grantCallId : {}", vo.getGrantCallId());
		logger.info("personId : {}", vo.getPersonId());
		logger.info("tab Name : {}", vo.getTabName());
		return grantCallIOIService.loadGrantCallIOIByGrantId(vo);
	}

	@RequestMapping(value = "/deleteIOIMember", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteIOIPerson(@RequestBody GrantCallIOIVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteIOIMember");
		logger.info("Grant Call Id : {}", vo.getGrantCallId() );
		logger.info("Grant Call IOI : {}", vo.getGrantCallIOIId());
		logger.info("IOIMember Id : {}",  vo.getGrantIOIMemberId());
		return grantCallIOIService.deleteIOIMember(vo);
	}

	@RequestMapping(value = "/deleteIOIListItem", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteIOHeader(@RequestBody GrantCallIOIVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteIOIListItem");
		logger.info("Grant Call Id : {}", vo.getGrantCallId() );
		logger.info("Grant Call IOI : {}", vo.getGrantCallIOIId());
		return grantCallIOIService.deleteIOIListItem(vo);
	}

	@RequestMapping(value = "/addIOIMember", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addKeyPerson(@RequestBody GrantCallIOIVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addIOIMember");
		logger.info("grantCallIOIId : {}", vo.getGrantCallIOIId());
		logger.info("personId : {}", vo.getPersonId());
		logger.info("updateUser : {}", vo.getUpdateUser());
		return grantCallIOIService.saveOrUpdateIOIMembers(vo);
	}
	
	@RequestMapping(value = "/exportIOIDatas", method = RequestMethod.POST)
	public ResponseEntity<byte[]> exportProposalDashboardData(HttpServletRequest request, @RequestBody GrantCallIOIVO vo) throws Exception {
		logger.info("Requesting for exportIOIDatas");
		XSSFWorkbook workbook = grantCallIOIService.getXSSFWorkbookForIOI(vo);
		return grantCallIOIService.getResponseEntityForIOIDownload(vo, workbook);
	}

}
