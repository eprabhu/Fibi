import { Component } from '@angular/core';
import { AttachmentTab, ComplianceTab, OverviewTabSection, SponsorTabSection, SubawardOrganisationTab } from '../shared/entity-constants';
import { jumpToSection } from '../../common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { Router } from '@angular/router';
import { EntityDataStoreService } from '../entity-data-store.service';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntityManagementService } from '../entity-management.service';
import { DataStoreEvent, EntireEntityDetails, EntityTabStatus } from '../shared/entity-interface';

@Component({
  selector: 'app-right-panel',
  templateUrl: './right-panel.component.html',
  styleUrls: ['./right-panel.component.scss']
})
export class RightPanelComponent {

    currentTab: 'OVERVIEW' | 'SPONSOR' | 'SUBAWARD' | 'COMPLIANCE' | 'ATTACHMENTS' | '';
    sectionDetails: any[] = [];
    selectedSectionId: any;
    $subscriptions: Subscription[] = [];
    dunsNumber: any;
    isDunsMatched: boolean;
    tabDetails = new EntityTabStatus();
    isEditMode: boolean;

    constructor(private _router: Router, private _dataStorService: EntityDataStoreService, public entityManagementService: EntityManagementService) {}

    ngOnInit() {
        this.routerEventSubscription();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    getCurrentTab(currentURL): any {
        if(currentURL.includes('entity-overview')) {
            return 'OVERVIEW';
        } else if (currentURL.includes('entity-sponsor')) {
            return 'SPONSOR';
        } else if (currentURL.includes('entity-subaward')) {
            return 'SUBAWARD';
        } else if (currentURL.includes('entity-compliance')) {
            return 'COMPLIANCE';
        } else if (currentURL.includes('entity-attachments')) {
            return 'ATTACHMENTS';
        } else {
            return '';
        }
    }

    private getDataFromStore() {
        const ENTITY_DATA: EntireEntityDetails = this._dataStorService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        this.dunsNumber = ENTITY_DATA?.entityDetails?.dunsNumber;
        this.isDunsMatched = ENTITY_DATA?.entityDetails?.isDunsMatched;
        this.tabDetails = ENTITY_DATA?.entityTabStatus;
        this.isEditMode = this._dataStorService.getEditMode();
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStorService.dataEvent.subscribe((dependencies: DataStoreEvent) => {
                this.getDataFromStore();
            })
        );
    }

    private getSectionDetails(): any[] {
        switch(this.currentTab) {
            case 'OVERVIEW': {
                return this.getArray(OverviewTabSection);
            }
            case 'SPONSOR': {
                return this.getArray(SponsorTabSection);
            }
            case 'SUBAWARD': {
                return this.getArray(SubawardOrganisationTab);
            }
            case 'COMPLIANCE': {
                return this.getArray(ComplianceTab);
            }
            case 'ATTACHMENTS': {
                return this.getArray(AttachmentTab);
            }
            default: {
                return [];
            }
        }
    }

    scrollToSelectedSection(section: any) {
        this.selectedSectionId = section.sectionId;
        this.entityManagementService.scrollToSelectedSection(this.selectedSectionId);
    }

    windowScroll(scrollTo: string) {
        const ELEMENT: HTMLElement = document.getElementById(scrollTo);
        const offsetFromHeader = document.getElementById('COI-DISCLOSURE-HEADER')?.clientHeight + 50;
        const sectionHeight = ELEMENT.offsetTop - offsetFromHeader;
        window.scrollTo({ behavior: 'smooth', top: sectionHeight });
    }

    getArray(map): any {
        let sectionName = [];
        map.forEach((value) => {
            sectionName.push(value);
        });
        return sectionName;
    }

    routerEventSubscription() {
        this.$subscriptions.push(this._router.events.subscribe(event => {
            this.currentTab  = this.getCurrentTab(window.location.href);
            this.sectionDetails = this.getSectionDetails();
            this.selectedSectionId = this.sectionDetails?.[0]?.sectionId;
        }));
      }

}
