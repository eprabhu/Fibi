package com.polus.appcoigraph.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.appcoigraph.dao.COIEntityGraphDao;
import com.polus.appcoigraph.dao.COIImportGraphDataDao;
import com.polus.appcoigraph.entity.COIEntity;
import com.polus.appcoigraph.model.RequestDTO;
import com.polus.appcoigraph.model.ResponseDTO;
import com.polus.appcoigraph.repository.EntityRepository;

@Service
public class COIGraphService {

	@Autowired
	private EntityRepository entityRepository;

	@Autowired
	private COIEntityGraphDao coiEntityGraphDao;
	
	@Autowired
	private COIImportGraphDataDao coiImportGraphDataDao;
	

    public List<COIEntity> getAllEntity() {
        return entityRepository.findAll();
    }    	
	   
	public void importDataFromRDBMS() {
		coiImportGraphDataDao.importDataFromRDBMS();		
	}	
		
	public ResponseDTO entityGraph(RequestDTO request) { 
		return coiEntityGraphDao.entityGraphDAO(request);
	}	
	
}
