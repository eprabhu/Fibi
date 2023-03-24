package com.polus.fibicomp.export.service;

import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.dashboard.vo.AwardDashboardVO;

@Service
public interface ExportDataService {

	public ResponseEntity<byte[]> getResponseEntityForActiveAward(AwardDashboardVO vo, XSSFWorkbook workbook) throws Exception;

	public XSSFWorkbook getXSSFWorkbookForActive(AwardDashboardVO vo) throws Exception;

}
