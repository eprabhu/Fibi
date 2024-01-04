import {Component, EventEmitter, Input, OnChanges, OnInit, Output} from '@angular/core';
import {slideHorizontal} from '../../../../../../fibi/src/app/common/utilities/animations';

@Component({
    selector: 'app-define-switch-view',
    templateUrl: './define-switch-view.component-slider.html',
    styleUrls: ['./define-switch-view.component-slider.scss'],
    animations: [slideHorizontal]
})
export class DefineSwitchViewComponentSlider implements OnInit, OnChanges {

    @Input() sfiList = [];
    @Input() projectSFIDetails: any = {};
    @Input() selectedSfiIndex = -1;
    @Input() coiStatusList = [];
    @Input() projectIdTitleMap = {};
    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();

    sfi: any = {};
    entityProjectDetails = [];
    selectedProject: any = {};
    clearIndex = 0;
    readMore = [];
    relationshipTypeCache = {};
    isDataChanged = false;

    ngOnInit() {
        setTimeout(() => this.showTaskNavBar());
    }

    ngOnChanges() {
        this.setCurrentIndexSfi();
    }

    setCurrentIndexSfi() {
        this.sfi = this.sfiList[this.selectedSfiIndex];
    }

    goBackStep() {
        this.selectedSfiIndex--;
        this.setCurrentIndexSfi();
    }

    goToStep() {
        this.selectedSfiIndex++;
        this.setCurrentIndexSfi();
    }

    getEntityRelationTypePills(validPersonEntityRelType: string) {
        if (validPersonEntityRelType) {
            if (this.relationshipTypeCache[validPersonEntityRelType]) {
                return this.relationshipTypeCache[validPersonEntityRelType];
            }
            const entityRelTypes = validPersonEntityRelType.split(':;:');
            this.relationshipTypeCache[validPersonEntityRelType] = entityRelTypes.map(entity => {
                const relationshipType = entity.split(':');
                return {relationshipType: relationshipType[0] || '', description: relationshipType[1] || ''};
            });
            return this.relationshipTypeCache[validPersonEntityRelType];
        }
    }

    readMoreOption(id: number, flag: boolean): void {
        this.readMore[id] = !flag;
    }

    validateProjectSfiSliderOnClose() {
        this.hideSlider();
    }

    hideSlider() {
        const slider = document.querySelector('.slider-base');
        slider.classList.remove('slider-opened');
        document.getElementById('COI_SCROLL').classList.remove('overflow-hidden');
        document.getElementById('COI_SCROLL').classList.add('overflow-y-scroll');
        setTimeout(() => this.closePage.next(this.isDataChanged), 500);
    }

    showTaskNavBar() {
        document.getElementById('COI_SCROLL').classList.add('overflow-hidden');
        const slider = document.querySelector('.slider-base');
        slider.classList.add('slider-opened');
    }

}
