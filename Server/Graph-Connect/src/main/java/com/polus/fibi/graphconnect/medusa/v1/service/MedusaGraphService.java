package com.polus.fibi.graphconnect.medusa.v1.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.fibi.graphconnect.coi.v1.dao.COIImportGraphDataDao;
import com.polus.fibi.graphconnect.medusa.v1.dao.MedusaGraphDao;
import com.polus.fibi.graphconnect.medusa.v1.dao.MedusaImportGraphDataDao;
import com.polus.fibi.graphconnect.model.RequestDTO;
import com.polus.fibi.graphconnect.model.ResponseDTO;

@Service
public class MedusaGraphService {

	@Autowired
	private MedusaGraphDao medusaGraphDao;

	@Autowired
	private COIImportGraphDataDao coiImportGraphDataDao;

	@Autowired
	private MedusaImportGraphDataDao medusaImportGraphDataDao; 	

	public void importDataFromRDBMS() {
		medusaImportGraphDataDao.importDataFromRDBMS();		
	}

	public void refreshDataFromRDBMS() {
		medusaImportGraphDataDao.refreshDataFromRDBMS();		
	}

	public ResponseDTO medusaGraph(RequestDTO request) { 
		return medusaGraphDao.getMedusaGraph(request);
	}	

}
