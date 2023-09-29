import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {OpaComponent} from './opa.component';
import {RouterModule, Routes} from '@angular/router';
import {MatIconModule} from '@angular/material/icon';
import {MatMenuModule} from '@angular/material/menu';
import {SharedComponentModule} from '../shared-components/shared-component.module';
import {SharedModule} from '../shared/shared.module';
import {FormBuilderViewComponent} from './form-builder-view/form-builder-view.component';

const routes: Routes = [{path: '', component: OpaComponent}];

@NgModule({
    declarations: [
        OpaComponent,
        FormBuilderViewComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        MatIconModule,
        MatMenuModule,
        SharedComponentModule,
        SharedModule
    ]
})
export class OpaModule {
}
