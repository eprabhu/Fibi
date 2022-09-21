package com.polus.fibicomp.mapmaintenance.service;

import java.util.ArrayList;
import java.util.HashMap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.mapmaintenance.dao.MapMaintenanceDao;
import com.polus.fibicomp.mapmaintenance.dto.MapMaintenanceDataBus;

@Service
public class MapMaintenanceService {

	@Autowired
	MapMaintenanceDao mapMaintenanceDao;

	/* Fetch Full Details of Maps */
	public MapMaintenanceDataBus getMapList(MapMaintenanceDataBus mapMaintenanceDataBus) {

		ArrayList<HashMap<String, Object>> mapMaintenanceList = mapMaintenanceDao.getMapList();
		mapMaintenanceDataBus.getMapMaintenance().setMap(mapMaintenanceList);
		return mapMaintenanceDataBus;
	}

	/* Fetch Details of Role description */
	public MapMaintenanceDataBus getRoleDescription(MapMaintenanceDataBus mapMaintenanceDataBus) {

		ArrayList<HashMap<String, Object>> roleDescription = mapMaintenanceDao.getRoleDescription();
		mapMaintenanceDataBus.setRoleDescription(roleDescription);
		return mapMaintenanceDataBus;
	}

	/* Fetch Details of Maps based on Id */
	public MapMaintenanceDataBus getMapDetailsId(MapMaintenanceDataBus mapMaintenanceDataBus) {
		ArrayList<HashMap<String, Object>> mapDetailList = mapMaintenanceDao
				.getMapDetailsById(mapMaintenanceDataBus.getMapId());
		ArrayList<HashMap<String, Object>> mapList = mapMaintenanceDao.getMapById(mapMaintenanceDataBus.getMapId());
		mapMaintenanceDataBus.getMapDetails().setMapDetailList(mapDetailList);
		mapMaintenanceDataBus.getMapDetails().setMapList(mapList);
		return mapMaintenanceDataBus;
	}

	/* Inactivate Map Details based on Id */
	public MapMaintenanceDataBus deleteMap(MapMaintenanceDataBus mapMaintenanceDataBus) {
		int mapId = mapMaintenanceDataBus.getMapId();
		Integer countOfMap = mapMaintenanceDao.isMapUsed(mapId);
		if (countOfMap == 0) {
			mapMaintenanceDao.deleteMap(mapId);
		}
		ArrayList<HashMap<String, Object>> mapMaintenanceList = mapMaintenanceDao.getMapList();
		mapMaintenanceDataBus.getMapMaintenance().setMap(mapMaintenanceList);
		mapMaintenanceDataBus.setCountOfMapUsed(countOfMap);
		return mapMaintenanceDataBus;
	}

	/* Insertion of Maps */
	public MapMaintenanceDataBus insertMap(MapMaintenanceDataBus mapMaintenanceDataBus) {

		ArrayList<HashMap<String, Object>> mapList = mapMaintenanceDataBus.getMapDetails().getMapList();
		ArrayList<HashMap<String, Object>> mapDetailList = mapMaintenanceDataBus.getMapDetails().getMapDetailList();
		try {
			int mapId = mapMaintenanceDao.getNextMapId();
			for (HashMap<String, Object> listMap : mapList) {
				int isInserted = mapMaintenanceDao.insertMap(listMap, mapId);
				if (isInserted == 1) {
					for (HashMap<String, Object> detailListMap : mapDetailList) {
						mapMaintenanceDao.insertMapDetail(detailListMap, mapId);
					}
				}
			}
		} catch (Exception e) {
			System.out.println("Exception in insertMap" + e.getMessage());
		}
		return mapMaintenanceDataBus;
	}

	/* Updation of Maps */
	public MapMaintenanceDataBus updateMap(MapMaintenanceDataBus mapMaintenanceDataBus) {

		ArrayList<HashMap<String, Object>> mapList = mapMaintenanceDataBus.getMapDetails().getMapList();
		ArrayList<HashMap<String, Object>> mapDetailList = mapMaintenanceDataBus.getMapDetails().getMapDetailList();

		try {
			if (mapMaintenanceDataBus.getMapDetails().getDeletedMapDetailList() != null) {
				for (Integer item : mapMaintenanceDataBus.getMapDetails().getDeletedMapDetailList()) {
					mapMaintenanceDao.deleteMapDetailsById(item);
				}
			}
			for (HashMap<String, Object> listMap : mapList) {
				int mapId = (int) listMap.get("MAP_ID");
				int isInserted = mapMaintenanceDao.updateMap(listMap);
				if (isInserted == 1) {
					for (HashMap<String, Object> detailListMap : mapDetailList) {
						int mapDetailId = (int) detailListMap.get("MAP_DETAIL_ID");
						if (mapDetailId == 0) {
							mapMaintenanceDao.insertMapDetail(detailListMap, mapId);

						} else {
							mapMaintenanceDao.updateMapDetail(detailListMap, mapId);
						}

					}
				}
			}
		} catch (Exception e) {
			System.out.println("Exception in Update Map" + e.getMessage());
		}
		return mapMaintenanceDataBus;
	}

	public MapMaintenanceDataBus getUnitList(MapMaintenanceDataBus mapMaintenanceDataBus) {
		ArrayList<HashMap<String, Object>> unitList = mapMaintenanceDao.getUnitList();
		mapMaintenanceDataBus.getUnitList().setUnitList(unitList);
		return mapMaintenanceDataBus;
	}

	public MapMaintenanceDataBus getRoleDescriptionByModuleCode(MapMaintenanceDataBus mapMaintenanceDataBus) {
		ArrayList<HashMap<String, Object>> roleDescription = mapMaintenanceDao.getRoleDescriptionByModuleCode(mapMaintenanceDataBus.getModuleCode(), mapMaintenanceDataBus.getSubModuleCode());
		mapMaintenanceDataBus.setRoleDescription(roleDescription);
		return mapMaintenanceDataBus;
	}

}
