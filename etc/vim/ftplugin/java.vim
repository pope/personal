python << EOF
import vim
from subprocess import Popen, PIPE

def update_classpath_from_maven():
    p1 = Popen(["mvn",
                "dependency:list",
                "-DoutputAbsoluteArtifactFilename=true"], stdout=PIPE)
    libs = []
    output = p1.communicate()[0]
    for line in output.split("\n"):
        pieces = line.split(":")
        if len(pieces) == 6:
            libs.append(pieces[5])
    libs.append("target/classes")
    libs.append("target/test-classes")

    vim.command("let g:vjde_lib_path='%s'" % ":".join(libs))
    vim.command("let g:vjde_src_path='src/main/java'")
    vim.command("let g:vjde_web_app='src/main/webapp'")
    vim.command("let g:vjde_out_path='target/classes'")
    vim.command("let g:vjde_test_path='src/test/java'")
    vim.command("let g:vjde_java_cfu={}")

EOF
map <Leader>jup :py update_classpath_from_maven()<CR>
