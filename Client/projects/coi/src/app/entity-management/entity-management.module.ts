import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {EntityManagementComponent} from './entity-management.component';
import {RouterModule, Routes} from "@angular/router";
import { EntityListComponent } from './entity-list/entity-list.component';

const routes: Routes = [
  { path: '', component: EntityManagementComponent },
  { path: 'entity-details', loadChildren: () => import('./entity-details/entity-details.module').then(m => m.EntityDetailsModule) }
];

@NgModule({
    declarations: [
        EntityManagementComponent,
        EntityListComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes)
    ],
    exports:[
    ]
})
export class EntityManagementModule {
}
