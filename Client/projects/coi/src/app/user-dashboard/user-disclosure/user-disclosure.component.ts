import { Component } from '@angular/core';
import { UserDisclosureService } from "./user-disclosure.service";
import { UserDashboardService } from "../user-dashboard.service";
import { CommonService } from "../../common/services/common.service";

@Component({
    selector: 'app-user-disclosure',
    templateUrl: './user-disclosure.component.html',
    styleUrls: ['./user-disclosure.component.scss']
})

export class UserDisclosureComponent {
    isShowCountModal = false;
    searchText = '';
    currentSelected = {
        tab: 'IN_PROGRESS_DISCLOSURES',
        filter: 'ALL',


    }
    dashboardRequestObject = {
        advancedSearch: 'L',
        pageNumber: 200,
        sort: { 'createTimestamp': 'asc' },
        tabName: 'IN_PROGRESS_DISCLOSURES',
        isDownload: false,
        // filterType = 'All', 'FCOI', 'Project', 'OPA';
        filterType: 'ALL'
    };
    disclosureArray: any[] = [];
    filteredDisclosureArray: any[] = [];
    dashboardCount: any;
    isActiveDisclosureAvailable = false;
    selectedModuleCode: number;
    currentDisclosureId: any;
    currentDisclosureNumber: any;
    disclosureType: any;
    inputType: string;
    coiList: [];
    ishover: [] = [];
    disclosureSequenceStatusCode: any;
    personId: any;
    onButtonHovering:any = true;
    index:any
    constructor(public userDisclosureService: UserDisclosureService,
        public userDashboardService: UserDashboardService,
        public commonService: CommonService) {
    }

    ngOnInit() {
        this.loadDashboard();
        this.loadDashboardCount();
       
        
    }

    loadDashboard() {
        this.userDisclosureService.getCOIDashboard(this.dashboardRequestObject).subscribe((res: any) => {
            this.disclosureArray = res.disclosureViews ? res.disclosureViews : [];
            this.searchText = '';
            this.setFilter();
        })
    }

    loadDashboardCount() {
        this.userDisclosureService.getCOIDashboardCount(this.dashboardRequestObject).subscribe((res: any) => {
            this.dashboardCount = res;
        })
    }


    getDispositionStatusBadge(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case '2':
            case '3': return 'success';
            case '4': return 'warning';
            default: return 'info';
        }
    }
    getDispositionStatusBadgeUpdated(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case 2: return 'success';
            case 3: return 'danger';
            case '4': return 'warning';
            default: return 'info';
        }
    }
    getDispositionStatusTextColor(statusCode) {
        switch (statusCode) {
            case '1': return 'black';
            case 2: return 'white';
            case 3: return 'white';
            default: return 'white';
        }
    }
    getDispositinTextColor(statusCode) {
        switch (statusCode) {
            case '1': return 'black';
            case '2':
            case '3': return 'white';
            case '4': return 'black';
            default: return 'white';
        }
    }

    getReviewStatusBadge(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case '2': return 'info';
            case '3': return 'success';
            case '4': return 'success';
            default: return 'danger';
        }
    }
    getReviewStatusTextColor(statusCode) {
        switch (statusCode) {
            case '1': return 'black';
            case '2': return 'white';
            case '3': return 'white';
            default: return 'white ';
        }
    }




    getDisclosureStatusBadge(statusCode) {
        switch (statusCode) {
            case 1: return 'warning';
            case 2:
            case 4:
            case 5:
                return 'info';
            case 3: case 6: return 'success';
            default: return 'danger';
        }
    }
    getDisclosureStatusText(statusCode) {
        switch (statusCode) {
            case 1: return 'black';
            case 2:
            case 4:
            case 5: case 3: case 6: return 'white';
            default: return 'black';
        }
    }

    getEventType(disclosureSequenceStatusCode, disclosureCategoryType) {
        if (disclosureCategoryType == 1) {
            if (disclosureSequenceStatusCode == 2 || disclosureSequenceStatusCode == 1 && !this.isActiveDisclosureAvailable) {
                return 'Active';
            } else if (disclosureSequenceStatusCode == 1 && this.isActiveDisclosureAvailable) {
                return 'Revision';
            }
        } else if (disclosureCategoryType == 3) {
            return 'Proposal';
        }
        
    }

    setTab(tabName) {
        this.currentSelected.tab = tabName;
        this.dashboardRequestObject.tabName = tabName;
        this.loadDashboard();
    }
    setSelectedModuleCode(moduleName, id, coiNumber, disSeqCode, personId,noOfcount) {
        if(noOfcount >0 ){
            switch (moduleName) {
                case 'sfi':
                    this.selectedModuleCode = 8;
                    break;
                case 'proposal' :
                    this.selectedModuleCode = 8;
                    break;
                case 'Awards' : 
                    this.selectedModuleCode = 8;  
                    break;
                default:
                    this.selectedModuleCode = 0;
            }
            this.isShowCountModal = true;
            this.currentDisclosureId = id;
            this.currentDisclosureNumber = coiNumber;
            this.disclosureType = moduleName;
            this.inputType = 'DISCLOSURE_TAB';
            this.disclosureSequenceStatusCode = disSeqCode;
            this.personId = personId;
        }

    }

    setFilter(type = 'ALL') {
        this.currentSelected.filter = type;
        this.dashboardRequestObject.filterType = type;
        this.filterDashboardData();
    }

    filterDashboardData() {
        if (this.currentSelected.filter == 'ALL') {
            this.filteredDisclosureArray = this.disclosureArray;
            // console.log(this.filteredDisclosureArray)
        } else {
            this.filteredDisclosureArray = this.disclosureArray.filter(disclosure => {
                switch (this.currentSelected.filter) {
                    case 'PROJECT':
                        return ['3', '2'].includes(disclosure.fcoiTypeCode);
                    case 'OPA':
                        return disclosure.fcoiTypeCode == '5';
                    case 'FCOI':
                        return disclosure.fcoiTypeCode == '1';
                }
            });
        }
    }
    closeModalEvent(event){
        if(!event){
            this.isShowCountModal = event;
        }
    }
}
