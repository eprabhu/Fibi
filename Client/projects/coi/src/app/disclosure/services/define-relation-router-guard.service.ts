import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot } from '@angular/router';
import { Observable, of, forkJoin, NextObserver } from 'rxjs';
import { catchError, map } from 'rxjs/operators';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { DataStoreService } from './data-store.service';
import { CommonService } from '../../common/services/common.service';
import { DefineRelationshipService } from '../define-relationship/services/define-relationship.service';
import { COI } from '../coi-interface';
import { DefineRelationshipDataStoreService } from '../define-relationship/services/define-relationship-data-store.service';

@Injectable()
export class DefineRelationsRouterGuard implements CanActivate {
    data = [
        {
            "coiDisclProjectId": 19917,
            "moduleCode": 3,
            "projectId": "26488",
            "projectNumber": "26488",
            "title": "Quantum Computing on Mars",
            "projectStatus": "Approved",
            "projectStartDate": 1649045000000,
            "projectEndDate": 1714449900000,
            "homeUnitNumber": "000002",
            "homeUnitName": "UTNASA",
            "sponsorName": "NASA",
            "piName": "Doe, John",
            "keyPersonId": "10000000002",
            "keyPersonName": "Johnson, Jill",
            "projectTypeCode": "3",
            "projectType": "Development Proposal",
            "projectBadgeColour": "#9e7d32",
            "projectIcon": "rocket_launch",
            conflictCount: {},
            "coiDisclEntProjDetails": [
                {
                    "coiDisclProjectEntityRelId": 465536,
                    "coiDisclProjectId": 19917,
                    "personEntityId": 220,
                    "personEntityNumber": 178,
                    "entityId": 29691,
                    "updatedBy": "10000000002",
                    "updateTimestamp": 1723045846000,
                    disclComment: {
                        comment: ''
                    },
                    "personEntity": {
                        "personEntityId": 220,
                        "entityId": 29691,
                        "entityName": "Cyberdyne",
                        "entityNumber": 29688,
                        "countryName": "USA",
                        "validPersonEntityRelType": "Financial: Partner",
                        "entityType": "Private",
                        "entityRiskCategory": "High",
                        "personEntityVersionStatus": "ACTIVE",
                        "isFormCompleted": true
                    }
                }
            ]
        },
        {
            "coiDisclProjectId": 19918,
            "moduleCode": 4,
            "projectId": "26489",
            "projectNumber": "26489",
            "title": "AI Research in Healthcare",
            "projectStatus": "In Progress",
            "projectStartDate": 1649146000000,
            "projectEndDate": 1714551000000,
            "homeUnitNumber": "000003",
            "homeUnitName": "UTMed",
            "sponsorName": "WHO",
            "piName": "Clark, Susan",
            "keyPersonId": "10000000003",
            "keyPersonName": "Brown, Bob",
            "projectTypeCode": "4",
            "projectType": "Clinical Trial",
            "projectBadgeColour": "#8d9e42",
            "projectIcon": "health_and_safety",
            conflictCount: {},
            "coiDisclEntProjDetails": [
                {
                    "coiDisclProjectEntityRelId": 465537,
                    "coiDisclProjectId": 19918,
                    "personEntityId": 221,
                    "personEntityNumber": 179,
                    "entityId": 29692,
                    "updatedBy": "10000000003",
                    "updateTimestamp": 1723145946000,
                    disclComment: {
                        comment: ''
                    },
                    "personEntity": {
                        "personEntityId": 221,
                        "entityId": 29692,
                        "entityName": "MediTech",
                        "entityNumber": 29689,
                        "countryName": "Canada",
                        "validPersonEntityRelType": "Financial: Consultant",
                        "entityType": "Publicly Owned",
                        "entityRiskCategory": "Medium",
                        "personEntityVersionStatus": "ACTIVE",
                        "isFormCompleted": true
                    }
                }
            ]
        },
        {
            "coiDisclProjectId": 19919,
            "moduleCode": 5,
            "projectId": "26490",
            "projectNumber": "26490",
            "title": "Renewable Energy Initiative",
            "projectStatus": "Pending Approval",
            "projectStartDate": 1649247000000,
            "projectEndDate": 1714652100000,
            "homeUnitNumber": "000004",
            "homeUnitName": "UTEnergy",
            "sponsorName": "DOE",
            conflictCount: {},
            "piName": "Miller, Dave",
            "keyPersonId": "10000000004",
            "keyPersonName": "Green, Amy",
            "projectTypeCode": "5",
            "projectType": "Grant",
            "projectBadgeColour": "#5d9e72",
            "projectIcon": "eco",
            "coiDisclEntProjDetails": [
                {
                    "coiDisclProjectEntityRelId": 465538,
                    "coiDisclProjectId": 19919,
                    "personEntityId": 222,
                    "personEntityNumber": 180,
                    "entityId": 29693,
                    "updatedBy": "10000000004",
                    "updateTimestamp": 1723246046000,
                    disclComment: {
                        comment: ''
                    },
                    "personEntity": {
                        "personEntityId": 222,
                        "entityId": 29693,
                        "entityName": "Green Energy Co",
                        "entityNumber": 29690,
                        "countryName": "Germany",
                        "validPersonEntityRelType": "Financial: Investor",
                        "entityType": "Private",
                        "entityRiskCategory": "Low",
                        "personEntityVersionStatus": "ACTIVE",
                        "isFormCompleted": true
                    }
                }
            ]
        },
        {
            "coiDisclProjectId": 19920,
            "moduleCode": 6,
            "projectId": "26491",
            "projectNumber": "26491",
            "title": "Space Exploration Program",
            "projectStatus": "Approved",
            "projectStartDate": 1649348000000,
            "projectEndDate": 1714753200000,
            "homeUnitNumber": "000005",
            "homeUnitName": "UTSpace",
            conflictCount: {},
            "sponsorName": "SpaceX",
            "piName": "Anderson, Mike",
            "keyPersonId": "10000000005",
            "keyPersonName": "White, Nancy",
            "projectTypeCode": "6",
            "projectType": "Contract",
            "projectBadgeColour": "#3d9e92",
            "projectIcon": "flight",
            "coiDisclEntProjDetails": [
                {
                    "coiDisclProjectEntityRelId": 465539,
                    "coiDisclProjectId": 19920,
                    "personEntityId": 223,
                    "personEntityNumber": 181,
                    "entityId": 29694,
                    "updatedBy": "10000000005",
                    "updateTimestamp": 1723346146000,
                    disclComment: {
                        comment: ''
                    },
                    "personEntity": {
                        "personEntityId": 223,
                        "entityId": 29694,
                        "entityName": "Interstellar Corp",
                        "entityNumber": 29691,
                        "countryName": "Japan",
                        "validPersonEntityRelType": "Financial: Advisor",
                        "entityType": "Publicly Owned",
                        "entityRiskCategory": "High",
                        "personEntityVersionStatus": "ACTIVE",
                        "isFormCompleted": true
                    }
                }
            ]
        },
        {
            "coiDisclProjectId": 19921,
            "moduleCode": 7,
            "projectId": "26492",
            "projectNumber": "26492",
            "title": "Marine Biology Study",
            "projectStatus": "In Progress",
            "projectStartDate": 1649449000000,
            "projectEndDate": 1714854300000,
            "homeUnitNumber": "000006",
            "homeUnitName": "UTMarine",
            "sponsorName": "NOAA",
            "piName": "Roberts, Lisa",
            "keyPersonId": "10000000006",
            "keyPersonName": "Taylor, Kevin",
            "projectTypeCode": "7",
            "projectType": "Research",
            conflictCount: {},
            "projectBadgeColour": "#2d9e42",
            "projectIcon": "science",
            "coiDisclEntProjDetails": [
                {
                    "coiDisclProjectEntityRelId": 465540,
                    "coiDisclProjectId": 19921,
                    "personEntityId": 224,
                    "personEntityNumber": 182,
                    "entityId": 29695,
                    "updatedBy": "10000000006",
                    "updateTimestamp": 1723446246000,
                    disclComment: {
                        comment: ''
                    },
                    "personEntity": {
                        "personEntityId": 224,
                        "entityId": 29695,
                        "entityName": "Oceanic Institute",
                        "entityNumber": 29692,
                        "countryName": "Australia",
                        "validPersonEntityRelType": "Financial: Consultant",
                        "entityType": "Private",
                        "entityRiskCategory": "Medium",
                        "personEntityVersionStatus": "ACTIVE",
                        "isFormCompleted": true
                    }
                }
            ]
        }
    ]

    constructor(
        private _dataStore: DataStoreService,
        private _commonService: CommonService,
        private _defineRelationService: DefineRelationshipService,
        private _defineRelationsDataStore: DefineRelationshipDataStoreService,
    ) { }

            // this.data.forEach((datas: any) => {
        //     datas.coiDisclEntProjDetails = new Array(200).fill(datas.coiDisclEntProjDetails[0])
        // });
        // this._defineRelationsDataStore.setStoreData([...new Array(200).fill(this.data[0]), ...this.data]);
        // console.log(this._defineRelationsDataStore.getStoreData())
        // return of(true)

        canActivate(route: ActivatedRouteSnapshot): Observable<boolean> {
            this._defineRelationService.coiStatusList = []; // Clear any previous status list
        
            return new Observable<boolean>((observer: NextObserver<boolean>) => {
                forkJoin([
                    this.getLookups(),
                    this.getProjectRelations()
                ]).subscribe((res: any) => {
                    if (res) {
                        observer.next(true);
                    } else {
                        observer.next(true); // Prevent navigation if results are not valid
                    }
                }, (_error: any) => {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                    observer.next(true); // Prevent navigation on error
                });
            });
        }
        
        private getLookups(): Observable<any> {
            return this._defineRelationService.lookups().pipe(
                map((res: any) => {
                    this._defineRelationService.coiStatusList = res.coiProjConflictStatusTypes;
                    return res; // Return the response for further processing in subscribe
                }),
                catchError((_error: any) => {
                    return of(null); // Return null on error
                })
            );
        }
        
        private getProjectRelations(): Observable<any> {
            const COI_DATA: COI = this._dataStore.getData();
            const PROJECT_SFI_RELATION = {
                disclosureId: COI_DATA.coiDisclosure.disclosureId,
                disclosureNumber: COI_DATA.coiDisclosure.disclosureNumber,
                personId: COI_DATA.coiDisclosure.person.personId
            };
        
            return this._defineRelationService.getProjectRelations(PROJECT_SFI_RELATION).pipe(
                map((res: any) => {
                    this._defineRelationsDataStore.setStoreData(res);
                    this._defineRelationService.configureScrollSpy();
                    return res; // Return the response for further processing in subscribe
                }),
                catchError((_error: any) => {
                    return of(null); // Return null on error
                })
            );
        }
        
}
