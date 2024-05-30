import { Component, OnInit } from '@angular/core';
import { CoiSummaryEventsAndStoreService } from '../coi-summary-events-and-store.service';
import { Subscription } from 'rxjs';
import { DataStoreService } from '../../services/data-store.service';
import { CommonService } from '../../../common/services/common.service';
import { openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { CoiService } from '../../services/coi.service';

@Component({
    selector: 'app-coi-review',
    templateUrl: './review.component.html',
    styleUrls: ['./review.component.scss']
})
export class ReviewComponent implements OnInit {

    isToolkitVisible = true;
    coiDetails: any = {};
    isRelationCollapsed = true;
    $subscriptions: Subscription[] = [];
    selectedProject: any = {};

    constructor(
        public _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        private _dataStore: DataStoreService, public commonService: CommonService, private _coiService: CoiService
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
        this.listenDataChangeFromStore();
        this.listenToolKitFocusSection();
    }

    fetchCOIDetails(): void {
        this.coiDetails = this._dataStoreAndEventsService.getData(
            this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId,
            ['coiDisclosure']
        ).coiDisclosure;
    }

    toggleToolkitVisibility(): void {
        this.isToolkitVisible = !this.isToolkitVisible;
        this._dataStoreAndEventsService.$isToolkitVisible.next(this.isToolkitVisible);
    }
    
    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.fetchCOIDetails();
            })
        );
    }
 
    openModuleSummaryDetails(event = null) {
        this.selectedProject = event;
        if (event) {
            openModal('projectViewModal');
        }
    }

    openProjectMoreDetails(moduleId) {
        let redirectUrl = '';
        if (this.selectedProject.moduleCode == 3) {
            redirectUrl = this.commonService.fibiApplicationUrl + '#/fibi/proposal/overview?proposalId=' + moduleId;
        } else {
            redirectUrl = this.commonService.fibiApplicationUrl + '#/fibi/award/overview?awardId=' + moduleId;
        }
        window.open(redirectUrl);
    }

    // 'COI803' parent element of Relationships header.
    private listenToolKitFocusSection() {
        this.$subscriptions.push(this._coiService.$isExpandSection.subscribe(ele =>{
            if (ele.section == 'COI803') {
                this.isRelationCollapsed = ele.isExpand;
            }
        }));
    }
    
}
