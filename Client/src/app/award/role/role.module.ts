import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RoleComponent } from './role.component';
import { RouterModule } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { RoleEditComponent } from './role-edit/role-edit.component';
import { RoleViewComponent } from './role-view/role-view.component';
import { SharedModule } from '../../shared/shared.module';
import { RoleService } from './role.service';


@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild([{ path: '', component: RoleComponent }]),
    FormsModule,
    SharedModule
  ],
  declarations: [RoleComponent, RoleEditComponent, RoleViewComponent],
  providers: [RoleService]
})
export class RoleModule { }
