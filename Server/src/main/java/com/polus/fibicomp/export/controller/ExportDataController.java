package com.polus.fibicomp.export.controller;

import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.dashboard.vo.AwardDashboardVO;
import com.polus.fibicomp.export.service.ExportDataService;

@RestController
public class ExportDataController {

	protected static Logger logger = LogManager.getLogger(ExportDataController.class.getName());

	@Autowired
	@Qualifier(value = "exportDataService")
	private ExportDataService exportDataService;

	@RequestMapping(value = "/exportActiveAwards", method = RequestMethod.POST)
	public ResponseEntity<byte[]> exportActiveAwards(HttpServletRequest request, @RequestBody AwardDashboardVO vo) throws Exception {
		logger.info("Requesting for exportActiveAwards");
		XSSFWorkbook workbook = exportDataService.getXSSFWorkbookForActive(vo);
		return exportDataService.getResponseEntityForActiveAward(vo, workbook);
	}

}
