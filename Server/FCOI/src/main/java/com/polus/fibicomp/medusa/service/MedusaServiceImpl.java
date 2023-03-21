package com.polus.fibicomp.medusa.service;

import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.HashMap;

import com.polus.fibicomp.medusa.dto.Medusa;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.medusa.dao.MedusaDao;
import com.polus.fibicomp.medusa.dto.MedusaDTO;

@Service(value = "medusaService")
public class MedusaServiceImpl implements MedusaService {

	protected static Logger logger = LogManager.getLogger(MedusaServiceImpl.class.getName());

	@Autowired
	public DBEngine dbEngine;

	@Autowired
	public MedusaDao medusaDao;

	@Autowired
	public CommonDao commonDao;

	@Override
	public MedusaDTO getMedusa(Integer moduleCode, String projectId) {
		return medusaDao.getMedusa(projectId, moduleCode);
	}

	@Override
	public String getMedusaMoreDetail(MedusaDTO vo) {
		HashMap<String, Object> detailsMap = medusaDao.getProjectDetailsSP(vo.getModuleName(), vo.getProjectNumber());
		String response = "";
		if (detailsMap != null) {
			response = commonDao.convertObjectToJSON(detailsMap);
		}
		return response;
	}
	@Override
	public String getModuleServiceRequestDetail(Medusa medusa){
		ArrayList serviceRequestData = medusaDao.getServiceRequestDetail(medusa.getModuleCode(), medusa.getProjectId(),0,null,null);
		return commonDao.convertObjectToJSON(serviceRequestData);
	}
}
