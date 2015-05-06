/*  Author: Joe Barrett
 *  Description: Creates a Payslip window and populates with
 *  information based on Staff properties
 *  Date Last Modified: 09/10/2014
 */
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

namespace Coursework
{
    public partial class Payslip : Window
    {
        Staff staff;
        public Payslip(Staff staff)
        {
            this.staff = staff;
            InitializeComponent();
        }

        // Hides window when close button pressed
        private void btnClose_Click(object sender, RoutedEventArgs e)
        {
            this.Hide();
        }

        // Populates fields with appropriate when payslip window is loaded
        private void wndPayslip_Loaded(object sender, RoutedEventArgs e)
        {
            Demonstrator demonstrator;
            if (staff.GetType() == typeof(Demonstrator))
            {
                demonstrator = (Demonstrator)staff;
                lblLastName.Content = demonstrator.SecondName;
                lblFirstName.Content = demonstrator.FirstName;
                lblDepartment.Content = "(" + demonstrator.Department + ")";
                lblHoursWorked.Content = demonstrator.HoursWorked;
                lblGrossPay.Content = "£" + (demonstrator.HoursWorked * (demonstrator.HourlyPayRate / 100)).ToString("0.00");
                lblTaxRate.Content = demonstrator.calcTaxRate((int)demonstrator.GrossPay) + "%";
                lblNetPay.Content = "£" + demonstrator.calcPay((int)demonstrator.HoursWorked);
                lblCourse.Content = demonstrator.Course;
                lblSupervisor.Content = demonstrator.Supervisor;

            }
            else
            {
                lblCourse.Visibility = Visibility.Hidden;
                lblSupervisor.Visibility = Visibility.Hidden;
                lblLastName.Content = staff.SecondName;
                lblFirstName.Content = staff.FirstName;
                lblDepartment.Content = "(" + staff.Department + ")";
                lblHoursWorked.Content = staff.HoursWorked;
                lblGrossPay.Content = "£" + (staff.HoursWorked * (staff.HourlyPayRate / 100)).ToString("0.00");
                lblTaxRate.Content = staff.calcTaxRate((int)staff.GrossPay) + "%";
                lblNetPay.Content = "£" + staff.calcPay((int)staff.HoursWorked);
            }
        }
    }
}
